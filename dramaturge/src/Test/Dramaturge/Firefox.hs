module Test.Dramaturge.Firefox where

import Control.Monad (when)
import Control.Monad.Extra (fromMaybeM)
import Data.Coerce (coerce)
import Data.Default (Default (..))
import Data.Int (Int32)
import Effectful
import Effectful.Dispatch.Static (unsafeEff_)
import Effectful.Exception (bracketOnError)
import Effectful.Marionette (Marionette)
import Effectful.Marionette qualified as Marionette
import Effectful.Process.Typed
    ( ExitCode (..)
    , Process
    , TypedProcess
    , getExitCode
    , getPid
    , proc
    , startProcess
    , waitExitCode
    )
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import System.Posix.Types (CPid (CPid))
import Test.Dramaturge.Log
import Prelude

data Config = Config
    { program :: FilePath
    -- ^ The path of the Firefox executable, or its name in the PATH.
    , headless :: Bool
    -- ^ Run Firefox without a visible UI. Defaults to 'True'.
    , closeOnError :: Bool
    -- ^ Stop the Firefox process if an exception is raised. Defaults to 'True'.
    , closeWhenDone :: Bool
    -- ^ Stop the Firefox process after the action completes normally. Defaults to 'True'.
    }
    deriving stock (Generic)

instance Default Config where
    def =
        Config
            { program = "firefox"
            , headless = True
            , closeOnError = True
            , closeWhenDone = True
            }

-- | Start a Firefox process with Marionette enabled. The caller is responsible for stopping the process.
startFirefox
    :: (TypedProcess :> es)
    => FilePath
    -> Bool
    -> Eff es (Process () () ())
startFirefox program headless =
    startProcess . proc program . mconcat $
        [ ["--marionette"]
        , ["--headless" | headless]
        ]

-- | Run an action with a managed Firefox process.
--
-- Firefox is started before the action and stopped afterward according to
-- the 'Config' flags.
withFirefox
    :: ( HasCallStack
       , TypedProcess :> es
       , Marionette :> es
       , Log :> es
       )
    => Config
    -> Eff es a
    -> Eff es a
withFirefox Config{..} action =
    bracketOnError
        ( do
            process <- startFirefox program headless
            pid <- unsafeEff_ (getPid process)
            logInfo "Starting Firefox process" (cpidJson <$> pid)
            pure process
        )
        ( \process -> when closeOnError do
            pid <- unsafeEff_ (getPid process)
            exitCode <- ensureStopped process
            logInfo
                "Stopped Firefox process on error"
                (cpidJson <$> pid, exitCodeJson exitCode)
        )
        \process -> do
            a <- action
            when closeWhenDone do
                pid <- unsafeEff_ (getPid process)
                exitCode <- ensureStopped process
                logInfo
                    "Done. Stopped Firefox process"
                    (cpidJson <$> pid, exitCodeJson exitCode)
            pure a
  where
    ensureStopped
        :: ( HasCallStack
           , TypedProcess :> es
           , Marionette :> es
           )
        => Process () () () -> Eff es ExitCode
    ensureStopped process =
        flip fromMaybeM (getExitCode process) do
            Marionette.quit
            waitExitCode process

    exitCodeJson :: ExitCode -> Int
    exitCodeJson ExitSuccess = 0
    exitCodeJson (ExitFailure i) = i

    cpidJson :: CPid -> Int32
    cpidJson = coerce
