module Test.Dramaturge.Firefox where

import Control.Monad (when)
import Control.Monad.Extra (unlessM)
import Data.Default (Default (..))
import Data.Maybe (isJust)
import Data.String (fromString)
import Effectful
import Effectful.Dispatch.Static (unsafeEff_)
import Effectful.Exception (bracketOnError)
import Effectful.Marionette (Marionette)
import Effectful.Marionette qualified as Marionette
import Effectful.Process.Typed
    ( Process
    , TypedProcess
    , checkExitCode
    , getExitCode
    , getPid
    , proc
    , startProcess
    )
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
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
            logInfo_ . fromString $ "Started Firefox process (" <> show pid <> ")"
            pure process
        )
        (when closeOnError . ensureStopped)
        \process -> do
            a <- action
            logInfo_ "Done"
            when closeWhenDone $ ensureStopped process
            pure a
  where
    ensureStopped
        :: ( HasCallStack
           , TypedProcess :> es
           , Marionette :> es
           , Log :> es
           )
        => Process () () () -> Eff es ()
    ensureStopped process =
        unlessM (isJust <$> getExitCode process) do
            pid <- unsafeEff_ (getPid process)
            Marionette.quit
            checkExitCode process
            logInfo_ . fromString $ "Stopped Firefox process (" <> show pid <> ")"
