module Test.Marionette.Selector where

import Data.Aeson.KeyMap qualified as Object
import Data.Aeson.Types
import Data.Hashable (Hashable)
import Data.Text (Text)
import GHC.Generics (Generic)
import Test.Marionette.Element (Element (..), Shadow (..))
import Prelude

-- | Specifies element(s) within a DOM tree using various selection methods.
data Selector
    = ById Text
    | ByName Text
    | -- | (Note: multiple classes are not allowed. For more control, use 'ByCSS')
      ByClass Text
    | ByTag Text
    | ByLinkText Text
    | ByPartialLinkText Text
    | ByCSS Text
    | ByXPath Text
    | Anon Text
    | AnonAttribute Text
    deriving stock (Generic, Eq, Show)
    deriving anyclass (Hashable)

selectorObject :: Selector -> Object
selectorObject s =
    case s of
        ById t -> selector "id" t
        ByName t -> selector "name" t
        ByClass t -> selector "class name" t
        ByTag t -> selector "tag name" t
        ByLinkText t -> selector "link text" t
        ByPartialLinkText t -> selector "partial link text" t
        ByCSS t -> selector "css selector" t
        ByXPath t -> selector "xpath" t
        Anon t -> selector "anon" t
        AnonAttribute t -> selector "anon attribute" t
  where
    selector :: Text -> Text -> Object
    selector sn t = Object.fromList ["using" .= sn, "value" .= t]

instance ToJSON Selector where
    toJSON = Object . selectorObject

data SelectorFrom = SelectorFrom Element Selector

instance ToJSON SelectorFrom where
    toJSON (SelectorFrom Element{..} s) = Object $ Object.fromList ["element" .= elementId] <> selectorObject s

data SelectorFromShadowRoot = SelectorFromShadowRoot Shadow Selector

instance ToJSON SelectorFromShadowRoot where
    toJSON (SelectorFromShadowRoot Shadow{..} s) = Object $ Object.fromList ["shadowRoot" .= shadowId] <> selectorObject s
