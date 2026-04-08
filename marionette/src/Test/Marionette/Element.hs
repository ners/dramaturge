module Test.Marionette.Element where

import Data.Aeson.Types
import Data.Hashable (Hashable)
import Data.Text (Text)
import Prelude

-- | An opaque identifier for a web page element.
newtype Element = Element {elementId :: Text}
    deriving newtype
        ( Eq
        , Ord
        , Show
        , Read
        , Hashable
        )

instance FromJSON Element where
    parseJSON = withObject "Element" \o -> do
        elementId <- o .: "element-6066-11e4-a52e-4f735466cecf"
        pure Element{..}

instance ToJSON Element where
    toJSON Element{..} = object ["element-6066-11e4-a52e-4f735466cecf" .= elementId]

-- | An opaque identifier for a web page element that identifies a shadow root.
newtype Shadow = Shadow {shadowId :: Text}
    deriving newtype
        ( Eq
        , Ord
        , Show
        , Read
        , Hashable
        )

instance FromJSON Shadow where
    parseJSON = withObject "Element" \o -> do
        shadowId <- o .: "shadow-6066-11e4-a52e-4f735466cecf"
        pure Shadow{..}

instance ToJSON Shadow where
    toJSON Shadow{..} = object ["shadow-6066-11e4-a52e-4f735466cecf" .= shadowId]
