module Test.Dramaturge.Firefox where

import Control.Monad (when)
import Data.Default (Default (..))
import Effectful
import Effectful.Exception (bracketOnError)
import Effectful.Process.Typed (TypedProcess, proc, startProcess, stopProcess)
import GHC.Generics (Generic)
import System.Process.Typed (Process)
import Prelude

data Config = Config
    { headless :: Bool
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
            { headless = True
            , closeOnError = True
            , closeWhenDone = True
            }

-- | Start a Firefox process with Marionette enabled. The caller is responsible for stopping the process.
startFirefox :: (TypedProcess :> es) => Bool -> Eff es (Process () () ())
startFirefox headless =
    startProcess . proc "firefox" . mconcat $
        [ ["--marionette"]
        , ["--headless" | headless]
        ]

-- | Run an action with a managed Firefox process.
--
-- Firefox is started before the action and stopped afterward according to
-- the 'Config' flags.
withFirefox :: (TypedProcess :> es) => Config -> Eff es a -> Eff es a
withFirefox Config{..} action =
    bracketOnError
        (startFirefox headless)
        (when closeOnError . stopProcess)
        $ (action <*) . when closeWhenDone . stopProcess
