module Test.Dramaturge where

import Control.Exception (bracket)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (ToJSON (toJSON))
import System.Process.Typed
import Test.Marionette.Class (Marionette)
import Test.Marionette.Commands (Element, executeScript)
import Prelude

startFirefox :: Bool -> IO (Process () () ())
startFirefox headless =
    startProcess . proc "firefox" . mconcat $
        [ ["--marionette"]
        , ["--headless" | headless]
        ]

stopFirefox :: Process () () () -> IO ()
stopFirefox = stopProcess

withFirefox :: Bool -> (Process () () () -> IO a) -> IO a
withFirefox headless = bracket (startFirefox headless) stopFirefox

scrollIntoView :: (Marionette m) => Element -> m ()
scrollIntoView element = executeScript "arguments[0].scrollIntoView({behavior: 'instant', block: 'nearest'})" [toJSON element]
