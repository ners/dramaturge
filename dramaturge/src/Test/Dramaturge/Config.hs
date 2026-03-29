module Test.Dramaturge.Config where

import Data.Default (Default (..))
import Effectful.Log (LogLevel (..))
import GHC.Generics (Generic)
import Test.Dramaturge.Firefox qualified as Firefox

data Config = Config
    { firefox :: Firefox.Config
    , logLevel :: LogLevel
    }
    deriving stock (Generic)

instance Default Config where
    def =
        Config
            { firefox = def
            , logLevel = LogInfo
            }
