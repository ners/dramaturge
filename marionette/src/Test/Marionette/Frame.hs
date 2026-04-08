module Test.Marionette.Frame where

import Data.Aeson.Types
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Test.Marionette.Element (Element (..))
import Prelude

data Frame
    = FrameIndex Int
    | FrameElement Element
    | TopFrame
    deriving stock (Generic, Eq, Show)
    deriving anyclass (Hashable)

instance FromJSON Frame where
    parseJSON = withObject "Frame" \o ->
        liftA2 (,) (o .:? "id") (o .:? "element") >>= \case
            (Just _, Just _) -> fail "conflicting frame identifiers"
            (Just index, Nothing) -> pure . FrameIndex $ index
            (Nothing, Just elementId) -> pure . FrameElement . Element $ elementId
            (Nothing, Nothing) -> pure TopFrame

instance ToJSON Frame where
    toJSON (FrameIndex index) = object ["id" .= index]
    toJSON (FrameElement Element{..}) = object ["element" .= elementId]
    toJSON TopFrame = object ["id" .= Null]
