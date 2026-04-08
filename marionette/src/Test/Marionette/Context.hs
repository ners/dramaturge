module Test.Marionette.Context where

import Data.Aeson.Types
import GHC.Generics (Generic)
import Prelude

-- | The browsing context for script execution.
data Context = ContentContext | ChromeContext
    deriving stock (Generic, Eq, Show)

instance FromJSON Context where
    parseJSON = withText "Context" \case
        "content" -> pure ContentContext
        "chrome" -> pure ChromeContext
        other -> fail $ "Unknown context: " <> show other

instance ToJSON Context where
    toJSON ContentContext = String "content"
    toJSON ChromeContext = String "chrome"
