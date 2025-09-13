{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Marionette.Client where

import Control.Exception (AssertionFailed (AssertionFailed))
import Control.Monad (forever, void, (<=<))
import Control.Monad.Catch (Exception (..), MonadCatch, MonadMask, MonadThrow, catchAll, throwM)
import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.Reader qualified as Reader
import Control.Monad.Reader.Class (MonadReader)
import Data.Aeson (AesonException (AesonException), FromJSON)
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Binary (Binary)
import Data.Binary qualified as Binary
import Data.Binary.Get qualified as Binary
import Data.ByteString (ByteString)
import Data.ByteString.Builder.Extra qualified as ByteString
import Data.Foldable (for_, traverse_)
import Data.Functor.Identity (Identity (..))
import Data.IntMap.Strict qualified as IntMap
import Debug.Trace (traceM)
import GHC.Stack (HasCallStack)
import Network.Simple.TCP (HostName, ServiceName, SockAddr, Socket, closeSock, connectSock, sendLazy)
import Network.Socket.ByteString (recv)
import Test.Marionette.Protocol
import UnliftIO
    ( MonadUnliftIO
    , TMVar
    , TQueue
    , async
    , bracket
    , link
    , mapConcurrently
    , race
    , race_
    , readTMVar
    )
import UnliftIO.Concurrent (forkIO, killThread, threadDelay)
import UnliftIO.Retry (constantDelay, limitRetriesByCumulativeDelay, recoverAll)
import UnliftIO.STM
    ( atomically
    , modifyTVar'
    , newEmptyTMVarIO
    , newTQueueIO
    , newTVarIO
    , putTMVar
    , readTQueue
    , stateTVar
    , writeTQueue
    )
import Prelude hiding (log)

fromEitherM :: (MonadThrow m) => Either String a -> m a
fromEitherM = either (throwM . AssertionFailed) pure

decodeMarionetteM :: (MonadThrow m, FromJSON a) => MarionetteMessage -> m a
decodeMarionetteM = fromEitherM . decodeMarionette

data SocketClosed = SocketClosed
    deriving stock (Show)
    deriving anyclass (Exception)

newtype DecodeError = DecodeError String
    deriving stock (Show)
    deriving anyclass (Exception)

incoming :: forall m a. (MonadUnliftIO m, MonadThrow m, Binary a) => Socket -> m (TQueue a)
incoming socket = do
    q <- newTQueueIO
    link =<< async (go (atomically . writeTQueue q) (newDecoder ""))
    pure q
  where
    newDecoder :: ByteString -> Binary.Decoder a
    newDecoder bs = Binary.runGetIncremental Binary.get `Binary.pushChunk` bs
    go _ (Binary.Fail _ _ err) = throwM . DecodeError $ err
    go f (Binary.Done rest _ a) = do
        f a
        go f $ newDecoder rest
    go f dec =
        go f . (dec `Binary.pushChunk`)
            =<< liftIO (recv socket ByteString.defaultChunkSize `catchAll` \_ -> throwM SocketClosed)

connect :: (MonadUnliftIO m) => HostName -> ServiceName -> ((Socket, SockAddr) -> m a) -> m a
connect host port = bracket (recoverAll (limitRetriesByCumulativeDelay 5_000_000 $ constantDelay 50_000) . const $ connectSock host port) (closeSock . fst)

mapQueue_ :: (MonadIO m) => (a -> m ()) -> TQueue a -> m ()
mapQueue_ f = forever . f <=< atomically . readTQueue

newtype MarionetteTimeout = MarionetteTimeout MarionetteMessage
    deriving stock (Show)
    deriving anyclass (Exception)

newtype UnexpectedResult = UnexpectedResult Result
    deriving stock (Show)
    deriving anyclass (Exception)

parseResult :: forall m a. (MonadThrow m, FromJSON a) => Result -> m (Either Error a)
parseResult =
    either (pure . Left) $
        either (throwM . AesonException) (pure . Right)
            . Aeson.parseEither (Aeson.parseJSON @a)

data CommandWithCallback = CommandWithCallback Command (TMVar Result)

newtype MarionetteT m a = MarionetteT {runMarionetteT :: ReaderT (TQueue CommandWithCallback) m a}
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadThrow
        , MonadCatch
        , MonadMask
        , MonadIO
        , MonadUnliftIO
        , MonadReader (TQueue CommandWithCallback)
        )

type role MarionetteT representational nominal

getSendQueue :: (Monad m) => MarionetteT m (TQueue CommandWithCallback)
getSendQueue = Reader.ask

class (Functor m) => MarionetteClient m where
    sendCommand :: (HasCallStack, FromJSON a) => Command -> m a

    sendCommands :: (HasCallStack, Traversable t, FromJSON a) => t Command -> m (t a)
    default sendCommands :: (HasCallStack, MonadUnliftIO m, Traversable t, FromJSON a) => t Command -> m (t a)
    sendCommands = mapConcurrently sendCommand

    sendCommands_ :: (HasCallStack, Traversable t) => t Command -> m ()
    sendCommands_ = void . sendCommands @_ @_ @Aeson.Value

    sendCommand_ :: (HasCallStack) => Command -> m ()
    sendCommand_ = sendCommands_ . Identity

instance (MonadUnliftIO m, MonadThrow m) => MarionetteClient (MarionetteT m) where
    sendCommand :: (HasCallStack, FromJSON a) => Command -> MarionetteT m a
    sendCommand command = do
        q <- Reader.ask
        result <- newEmptyTMVarIO
        atomically . writeTQueue q $ CommandWithCallback command result
        either throwM pure =<< parseResult =<< atomically (readTMVar result)

runMarionette :: forall m a. (MonadUnliftIO m, MonadMask m) => MarionetteT m a -> m a
runMarionette action = do
    sendQueue :: TQueue CommandWithCallback <- newTQueueIO
    pendingCommands <- newTVarIO mempty
    let handleIncoming :: MarionetteMessage -> MarionetteT m ()
        handleIncoming message = do
            traceM $ "< " <> show message
            decodeMarionetteM message >>= \Message{..} ->
                atomically (stateTVar pendingCommands $ IntMap.updateLookupWithKey (\_ _ -> Nothing) messageId) >>= \case
                    Nothing -> throwM . UnexpectedResult $ messageContent
                    Just (result, timeout) -> do
                        killThread timeout
                        atomically $ putTMVar result messageContent
    let handleCommand :: Socket -> Int -> CommandWithCallback -> MarionetteT m ()
        handleCommand socket messageId (CommandWithCallback messageContent callback) = do
            let message = MarionetteMessage . Aeson.encode $ Message{..}
            traceM $ "> " <> show message
            liftIO . sendLazy socket . Binary.encode $ message
            timeout <- forkIO do
                threadDelay 5_000_000
                throwM . MarionetteTimeout $ message
            atomically . modifyTVar' pendingCommands $ IntMap.insert messageId (callback, timeout)
    let runSocket =
            connect "localhost" "2828" \(socket, _) -> do
                incomingQueue <- incoming socket
                void . decodeMarionetteM @_ @Greeting =<< atomically (readTQueue incomingQueue)
                let send = for_ [1 ..] \messageId -> handleCommand socket messageId =<< atomically (readTQueue sendQueue)
                let receive = void . forever $ handleIncoming =<< atomically (readTQueue incomingQueue)
                race_ send receive
    flip runReaderT sendQueue . runMarionetteT $
        either (const $ throwM SocketClosed) pure =<< race runSocket action
