module Test.Marionette.Cookie where

import Data.Aeson.Types
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude

data Cookie = Cookie
    { name :: Text
    , value :: Text
    , path :: Maybe Text
    , domain :: Maybe Text
    , secure :: Maybe Bool
    , httpOnly :: Maybe Bool
    , expiry :: Maybe Int
    }
    deriving stock (Generic, Show, Eq)
    deriving anyclass (FromJSON)

instance ToJSON Cookie where
    toJSON = genericToJSON defaultOptions{omitNothingFields = True}
