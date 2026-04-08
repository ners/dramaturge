module Test.Marionette.Orientation where

import Data.Aeson.Types
import GHC.Generics (Generic)
import Prelude

data Orientation = Portrait | Landscape
    deriving stock (Eq, Show, Generic)

instance ToJSON Orientation where
    toJSON Portrait = String "portrait"
    toJSON Landscape = String "landscape"

instance FromJSON Orientation where
    parseJSON = withText "Orientation" \case
        "portrait" -> pure Portrait
        "landscape" -> pure Landscape
        other -> fail $ "Unknown orientation: " <> show other
