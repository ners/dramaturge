module Test.Marionette.Window where

import Data.Aeson.Types
import Data.Hashable (Hashable)
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude

-------------------------------------------------------------------

newtype WindowHandle = WindowHandle Text
    deriving newtype
        ( Eq
        , Ord
        , Show
        , Read
        , Hashable
        )

instance FromJSON WindowHandle where
    parseJSON = withText "WindowHandle" $ pure . WindowHandle

instance ToJSON WindowHandle where
    toJSON (WindowHandle handle) = object ["handle" .= handle]

-------------------------------------------------------------------

data WindowType = Tab | Window
    deriving stock (Eq, Show, Generic)

instance ToJSON WindowType where
    toJSON Tab = String "tab"
    toJSON Window = String "window"

instance FromJSON WindowType where
    parseJSON = withText "WindowType" \case
        "tab" -> pure Tab
        "window" -> pure Window
        other -> fail $ "Unknown window type: " <> show other

-------------------------------------------------------------------

data NewWindowResult = NewWindowResult
    { newWindowHandle :: WindowHandle
    , newWindowType :: WindowType
    }
    deriving stock (Show, Eq, Generic)

instance FromJSON NewWindowResult where
    parseJSON = withObject "NewWindowResult" \o -> do
        newWindowHandle <- o .: "handle"
        newWindowType <- o .: "type"
        pure NewWindowResult{..}
