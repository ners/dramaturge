module Test.Dramaturge.Firefox where

import Effectful
import Effectful.Exception (bracket)
import Effectful.Process.Typed (TypedProcess, proc, startProcess, stopProcess)
import System.Process.Typed (Process)
import Prelude

startFirefox :: (TypedProcess :> es) => Bool -> Eff es (Process () () ())
startFirefox headless =
    startProcess . proc "firefox" . mconcat $
        [ ["--marionette"]
        , ["--headless" | headless]
        ]

stopFirefox :: (TypedProcess :> es) => Process () () () -> Eff es ()
stopFirefox = stopProcess

withFirefox :: (TypedProcess :> es) => Bool -> Eff es a -> Eff es a
withFirefox headless = bracket (startFirefox headless) stopFirefox . const
