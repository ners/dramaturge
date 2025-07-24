{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Dramaturge.Log
    ( Log
    , runLog
    , logTrace
    , logTrace_
    , logTraceShow_
    , logInfo
    , logInfo_
    , logInfoShow_
    , logAttention
    , logAttention_
    , logAttentionShow_
    )
where

import Control.Monad (forM_, when)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), Value, fromJSON)
import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy.Char8 qualified as LazyByteString
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Format.ISO8601 (iso8601Show)
import Effectful (Eff, IOE, MonadUnliftIO, withRunInIO, (:>))
import Effectful.Log (Log, LogLevel (..), LogMessage (..), Logger)
import Effectful.Log qualified
import GHC.Exception (prettyCallStackLines)
import GHC.Generics (Generic)
import GHC.Stack
    ( CallStack
    , HasCallStack
    , SrcLoc (..)
    , callStack
    , fromCallSiteList
    , getCallStack
    , withFrozenCallStack
    )
import Log.Internal.Logger qualified
import System.Console.ANSI
    ( Color (Yellow)
    , ColorIntensity (Dull)
    , ConsoleIntensity (..)
    , ConsoleLayer (Foreground)
    , SGR (..)
    , setSGR
    )
import System.IO.Unsafe (unsafePerformIO)
import UnliftIO.STM (atomically, newTMVarIO, putTMVar, takeTMVar)
import Prelude

instance FromJSON SrcLoc

instance ToJSON SrcLoc

instance FromJSON CallStack where
    parseJSON = fmap fromCallSiteList . parseJSON

instance ToJSON CallStack where
    toJSON = toJSON . getCallStack

data LogData = LogData
    { callStack :: Maybe CallStack
    , value :: Maybe Value
    }
    deriving stock (Generic)
    deriving anyclass (FromJSON, ToJSON)

withLogger :: (MonadUnliftIO m) => LogLevel -> (Logger -> m r) -> m r
withLogger logLevel act = withRunInIO \unlift -> do
    logger <- Effectful.Log.mkLogger "stdout" \LogMessage{..} -> do
        atomically $ takeTMVar lock
        putStrLn . unwords $
            [ iso8601Show lmTime
            , ""
            , showLogLevel lmLevel
            , ""
            , Text.unpack lmMessage
            ]
        case fromJSON lmData of
            Aeson.Success LogData{..} -> do
                forM_ value \value -> do
                    setSGR [SetColor Foreground Dull Yellow]
                    LazyByteString.putStrLn . encodePretty $ value
                    setSGR [Reset]
                when (logLevel >= LogTrace) . forM_ callStack $ \callStack -> do
                    setSGR [SetConsoleIntensity FaintIntensity]
                    mapM_ putStrLn . drop 1 . prettyCallStackLines $ callStack
                    putStrLn ""
                    setSGR [Reset]
            _ -> pure ()
        atomically $ putTMVar lock ()
    Log.Internal.Logger.withLogger logger (unlift . act)
  where
    lock = unsafePerformIO $ newTMVarIO ()
    showLogLevel :: LogLevel -> String
    showLogLevel LogInfo = "     INFO"
    showLogLevel LogTrace = "    TRACE"
    showLogLevel LogAttention = "ATTENTION"

runLog :: (IOE :> es) => LogLevel -> Eff (Log ': es) a -> Eff es a
runLog logLevel action = withLogger logLevel \logger -> Effectful.Log.runLog "hs-test" logger logLevel action

logMessage :: (HasCallStack, Log :> es) => LogLevel -> Text -> Value -> Eff es ()
logMessage logLevel t v =
    Effectful.Log.logMessage logLevel t $
        toJSON
            LogData
                { callStack = Just GHC.Stack.callStack
                , value = Just v
                }

logMessage_ :: (HasCallStack, Log :> es) => LogLevel -> Text -> Eff es ()
logMessage_ logLevel t =
    Effectful.Log.logMessage logLevel t $
        toJSON
            LogData
                { callStack = Just GHC.Stack.callStack
                , value = Nothing
                }

logTrace :: (HasCallStack, ToJSON a, Log :> es) => Text -> a -> Eff es ()
logTrace t = withFrozenCallStack $ logMessage LogTrace t . toJSON

logTrace_ :: (HasCallStack, Log :> es) => Text -> Eff es ()
logTrace_ = withFrozenCallStack $ logMessage_ LogTrace

logTraceShow_ :: (HasCallStack, Show a, Log :> es) => a -> Eff es ()
logTraceShow_ = withFrozenCallStack $ logTrace_ . Text.strip . Text.pack . show

logInfo :: (HasCallStack, ToJSON a, Log :> es) => Text -> a -> Eff es ()
logInfo t = withFrozenCallStack $ logMessage LogInfo t . toJSON

logInfo_ :: (HasCallStack, Log :> es) => Text -> Eff es ()
logInfo_ = withFrozenCallStack $ logMessage_ LogInfo

logInfoShow_ :: (HasCallStack, Show a, Log :> es) => a -> Eff es ()
logInfoShow_ = withFrozenCallStack $ logInfo_ . Text.strip . Text.pack . show

logAttention :: (HasCallStack, ToJSON a, Log :> es) => Text -> a -> Eff es ()
logAttention t = withFrozenCallStack $ logMessage LogAttention t . toJSON

logAttention_ :: (HasCallStack, Log :> es) => Text -> Eff es ()
logAttention_ = withFrozenCallStack $ logMessage_ LogAttention

logAttentionShow_ :: (HasCallStack, Show a, Log :> es) => a -> Eff es ()
logAttentionShow_ = withFrozenCallStack $ logAttention_ . Text.strip . Text.pack . show
