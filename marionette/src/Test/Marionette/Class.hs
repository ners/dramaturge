module Test.Marionette.Class where

import Control.Monad.Error.Class (MonadError)
import Test.Marionette.Client (MarionetteClient)
import Test.Marionette.Protocol (Error)

type Marionette m = (MarionetteClient m, MonadError Error m)
