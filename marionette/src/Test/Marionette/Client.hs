{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Marionette.Client where

import Control.Exception (AssertionFailed (AssertionFailed))
import Control.Monad (forever, void, (<=<), guard, unless, when)
import Control.Monad.Catch (Exception (..), MonadMask, MonadThrow, throwM, catchAll)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.Reader qualified as Reader
import Data.Aeson (FromJSON)
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Binary (Binary)
import Data.Binary qualified as Binary
import Data.Binary.Get qualified as Binary
import Data.ByteString (ByteString)
import Data.ByteString.Builder.Extra qualified as ByteString
import Data.Foldable (for_)
import Data.Functor.Identity (Identity (..))
import Data.IntMap.Strict qualified as IntMap
import Debug.Trace (traceM)
import Network.Simple.TCP (Socket, sendLazy, HostName, ServiceName, connectSock, closeSock, SockAddr)
import Test.Marionette.Class
import Test.Marionette.Protocol
import UnliftIO
    ( MonadUnliftIO
    , TQueue
    , race
    , race_, finally, bracket, catch, tryTakeMVar, handle, isEmptyTMVar, link, async
    )
import UnliftIO.Concurrent (forkIO, killThread, threadDelay)
import UnliftIO.STM
    ( atomically
    , modifyTVar'
    , newEmptyTMVarIO
    , newTQueueIO
    , newTVarIO
    , putTMVar
    , readTQueue
    , stateTVar
    , takeTMVar
    , writeTQueue
    )
import Prelude hiding (log)
import UnliftIO.Retry (recoverAll, constantDelay, limitRetriesByCumulativeDelay)
import Network.Socket.ByteString (recv)
import Data.ByteString qualified as ByteString

fromEitherM :: (MonadThrow m) => Either String a -> m a
fromEitherM = either (throwM . AssertionFailed) pure

decodeMarionetteM :: (MonadThrow m, FromJSON a) => MarionetteMessage -> m a
decodeMarionetteM = fromEitherM . decodeMarionette

parseM :: (MonadThrow m, FromJSON a) => Aeson.Value -> m a
parseM = fromEitherM . Aeson.parseEither Aeson.parseJSON

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

runMarionette :: forall m a. (MonadUnliftIO m, MonadMask m) => MarionetteT m a -> m a
runMarionette action = do
    sendQueue :: TQueue (CommandWithCallback (MarionetteT m)) <- newTQueueIO
    pendingCommands <- newTVarIO mempty
    let handleIncoming :: MarionetteMessage -> MarionetteT m ()
        handleIncoming message = do
            traceM $ "< " <> show message
            decodeMarionetteM message >>= \Message{..} ->
                atomically (stateTVar pendingCommands $ IntMap.updateLookupWithKey (\_ _ -> Nothing) messageId) >>= \case
                    Nothing -> throwM . UnexpectedResult $ messageContent
                    Just (f, timeout) -> killThread timeout >> f messageContent
    let handleCommand :: Socket -> Int -> CommandWithCallback (MarionetteT m) -> MarionetteT m ()
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
    flip runReaderT MarionetteState{sessionId = Nothing, ..} . runMarionetteT $
        either (const $ throwM SocketClosed) pure =<< race runSocket action

sendCommands' :: (Marionette m, Traversable t) => t (CommandWithCallback m) -> m ()
sendCommands' commands = atomically . for_ commands . writeTQueue =<< Reader.asks sendQueue

sendCommand' :: (Marionette m) => CommandWithCallback m -> m ()
sendCommand' command = atomically . flip writeTQueue command =<< Reader.asks sendQueue

sendCommands
    :: forall m t a
     . ( Marionette m
       , Traversable t
       , FromJSON a
       )
    => t Command
    -> m (t a)
sendCommands =
    mapM
        ( parseM
            <=< fromResultM
            <=< atomically . takeTMVar
        )
        <=< mapM \command -> do
            result <- newEmptyTMVarIO
            sendCommand' . CommandWithCallback command $ atomically . putTMVar result
            pure result

sendCommands_ :: (Marionette m, Traversable t) => t Command -> m ()
sendCommands_ = void . sendCommands @_ @_ @Aeson.Value

sendCommand :: (Marionette m, FromJSON a) => Command -> m a
sendCommand = fmap runIdentity . sendCommands . Identity

sendCommand_ :: (Marionette m) => Command -> m ()
sendCommand_ = sendCommands_ . Identity
