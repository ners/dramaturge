module Test.Marionette.AccessibilityProperties where

import Data.Aeson.Types (FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude

data AccessibilityProperties = AccessibilityProperties
    { role :: Maybe Text
    , name :: Maybe Text
    , value :: Maybe Text
    , description :: Maybe Text
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON)
