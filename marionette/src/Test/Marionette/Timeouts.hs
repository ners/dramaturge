module Test.Marionette.Timeouts where

import Data.Aeson.Types
import GHC.Generics (Generic)
import Prelude

data Timeouts = Timeouts
    { script :: Maybe Int
    , pageLoad :: Maybe Int
    , implicit :: Maybe Int
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON)

instance ToJSON Timeouts where
    toJSON = genericToJSON defaultOptions{omitNothingFields = True}
