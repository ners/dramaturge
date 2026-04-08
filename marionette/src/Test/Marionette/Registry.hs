module Test.Marionette.Registry where

import Data.Aeson.Types
import Data.Maybe (maybeToList)
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude

-------------------------------------------------------------------

data RegistryEntry = RegistryEntry
    { entryType :: Text
    , namespace :: Text
    , directory :: Text
    , options :: Maybe Text
    }
    deriving stock (Generic, Show, Eq)

instance ToJSON RegistryEntry where
    toJSON RegistryEntry{..} = toJSON $ [entryType, namespace, directory] <> maybeToList options
