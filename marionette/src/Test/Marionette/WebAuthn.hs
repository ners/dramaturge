module Test.Marionette.WebAuthn where

import Data.Aeson.Types
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude

-------------------------------------------------------------------

newtype AuthenticatorId = AuthenticatorId Text
    deriving stock (Show, Eq, Generic)
    deriving newtype (FromJSON, ToJSON)

-------------------------------------------------------------------

newtype CredentialId = CredentialId Text
    deriving stock (Show, Eq, Generic)
    deriving newtype (FromJSON, ToJSON)

-------------------------------------------------------------------

data VirtualAuthenticator = VirtualAuthenticator
    { protocol :: Text
    -- ^ "ctap1/u2f" or "ctap2"
    , transport :: Text
    -- ^ "usb", "nfc", "ble", "smart-card", "hybrid", or "internal"
    , hasResidentKey :: Bool
    , hasUserVerification :: Bool
    , isUserConsenting :: Bool
    , isUserVerified :: Bool
    , extensions :: Maybe [Text]
    , uvm :: Maybe Bool
    }
    deriving stock (Show, Eq, Generic)

instance ToJSON VirtualAuthenticator where
    toJSON = genericToJSON defaultOptions{omitNothingFields = True}

-------------------------------------------------------------------

data Credential = Credential
    { credentialId :: CredentialId
    , isResidentCredential :: Bool
    , rpId :: Text
    , privateKey :: Text
    , userHandle :: Maybe Text
    , signCount :: Int
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON)

instance ToJSON Credential where
    toJSON = genericToJSON defaultOptions{omitNothingFields = True}
