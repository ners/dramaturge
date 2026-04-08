module Test.Marionette.Rect where

import Data.Aeson.Types (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Prelude

data Rect = Rect
    { x :: Float
    , y :: Float
    , width :: Float
    , height :: Float
    }
    deriving stock (Generic, Show, Eq)
    deriving anyclass (FromJSON, ToJSON)
