{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Marionette.Commands where

import Control.Exception (AssertionFailed (AssertionFailed))
import Control.Monad.Catch (MonadThrow, throwM)
import Data.Aeson (FromJSON (parseJSON), KeyValue (..), ToJSON (toJSON), Value (..), (.:))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Base64 qualified as Base64
import Data.Foldable qualified as Foldable
import Data.Text
import Data.Text.Encoding qualified as Text
import GHC.Generics (Generic)
import GHC.IsList (IsList (Item, fromList))
import Test.Marionette.Class
import Test.Marionette.Client
import Test.Marionette.Protocol
import Prelude hiding (log)

newtype ValueObject a = ValueObject {value :: a}
    deriving stock (Generic)
    deriving anyclass (FromJSON, ToJSON)

type role ValueObject representational

-- | Specifies element(s) within a DOM tree using various selection methods.
data Selector
    = ById Text
    | ByName Text
    | -- | (Note: multiple classes are not
      -- allowed. For more control, use 'ByCSS')
      ByClass Text
    | ByTag Text
    | ByLinkText Text
    | ByPartialLinkText Text
    | ByCSS Text
    | ByXPath Text
    deriving stock (Eq, Show, Ord)

selectorObject :: Selector -> Aeson.Object
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
  where
    selector :: Text -> Text -> Aeson.Object
    selector sn t = fromList ["using" .= sn, "value" .= t]

instance ToJSON Selector where
    toJSON = Aeson.Object . selectorObject

-- | An opaque identifier for a web page element.
newtype Element = Element {elementId :: Text}
    deriving stock (Eq, Ord, Show, Read)

instance FromJSON Element where
    parseJSON = Aeson.withObject "Element" \o ->
        Element <$> (o .: "element-6066-11e4-a52e-4f735466cecf")

instance ToJSON Element where
    toJSON (Element e) = Aeson.object ["element-6066-11e4-a52e-4f735466cecf" .= e]

--   "Marionette:AcceptConnections": GeckoDriver.prototype.acceptConnections,
--   "Marionette:GetContext": GeckoDriver.prototype.getContext,
--   "Marionette:GetScreenOrientation": GeckoDriver.prototype.getScreenOrientation,
--   "Marionette:GetWindowType": GeckoDriver.prototype.getWindowType,

data SelectorFrom = SelectorFrom Element Selector

instance ToJSON SelectorFrom where
    toJSON (SelectorFrom Element{..} s) = Aeson.Object $ fromList ["element" .= elementId] <> selectorObject s

quit :: (Marionette m) => m ()
quit = sendCommand_ Command{command = "Marionette:Quit", parameters = Aeson.object []}

--   "Marionette:RegisterChromeHandler":
--     GeckoDriver.prototype.registerChromeHandler,
--   "Marionette:UnregisterChromeHandler":
--     GeckoDriver.prototype.unregisterChromeHandler,
--   "Marionette:SetContext": GeckoDriver.prototype.setContext,
--   "Marionette:SetScreenOrientation": GeckoDriver.prototype.setScreenOrientation,
--
--   // Addon service
--   "Addon:Install": GeckoDriver.prototype.installAddon,
--   "Addon:Uninstall": GeckoDriver.prototype.uninstallAddon,
--
--   // L10n service
--   "L10n:LocalizeProperty": GeckoDriver.prototype.localizeProperty,
--
--   // Reftest service
--   "reftest:setup": GeckoDriver.prototype.setupReftest,
--   "reftest:run": GeckoDriver.prototype.runReftest,
--   "reftest:teardown": GeckoDriver.prototype.teardownReftest,

acceptAlert :: (Marionette m) => m ()
acceptAlert = sendCommand_ Command{command = "WebDriver:AcceptAlert", parameters = Aeson.object []}

acceptDialog :: (Marionette m) => m ()
acceptDialog = sendCommand_ Command{command = "WebDriver:AcceptDialog", parameters = Aeson.object []}

addCookie :: (Marionette m) => m ()
addCookie = sendCommand_ Command{command = "WebDriver:AddCookie", parameters = Aeson.object []}

back :: (Marionette m) => m ()
back = sendCommand_ Command{command = "WebDriver:Back", parameters = Aeson.object []}

closeChromeWindow :: (Marionette m) => m ()
closeChromeWindow = sendCommand_ Command{command = "WebDriver:CloseChromeWindow", parameters = Aeson.object []}

closeWindow :: (Marionette m) => m ()
closeWindow = sendCommand_ Command{command = "WebDriver:CloseWindow", parameters = Aeson.object []}

deleteAllCookies :: (Marionette m) => m ()
deleteAllCookies = sendCommand_ Command{command = "WebDriver:DeleteAllCookies", parameters = Aeson.object []}

deleteCookie :: (Marionette m) => m ()
deleteCookie = sendCommand_ Command{command = "WebDriver:DeleteCookie", parameters = Aeson.object []}

deleteSession :: (Marionette m) => m ()
deleteSession = sendCommand_ Command{command = "WebDriver:DeleteSession", parameters = Aeson.object []}

dismissAlert :: (Marionette m) => m ()
dismissAlert = sendCommand_ Command{command = "WebDriver:DismissAlert", parameters = Aeson.object []}

elementClear :: (Marionette m) => m ()
elementClear = sendCommand_ Command{command = "WebDriver:ElementClear", parameters = Aeson.object []}

elementClick :: (Marionette m) => Element -> m ()
elementClick Element{..} =
    sendCommand_
        Command
            { command = "WebDriver:ElementClick"
            , parameters = Aeson.object ["id" .= elementId]
            }

elementSendKeys :: (Marionette m) => Element -> Text -> m ()
elementSendKeys Element{..} text =
    sendCommand_
        Command
            { command = "WebDriver:ElementSendKeys"
            , parameters = Aeson.object ["id" .= elementId, "text" .= text]
            }

executeAsyncScript :: (Marionette m) => m ()
executeAsyncScript = sendCommand_ Command{command = "WebDriver:ExecuteAsyncScript", parameters = Aeson.object []}

executeScript :: (Marionette m, Foldable f, FromJSON a) => Text -> f Value -> m a
executeScript script args =
    value
        <$> sendCommand
            Command
                { command = "WebDriver:ExecuteScript"
                , parameters = Aeson.object ["script" .= script, "args" .= Foldable.toList args]
                }

findElement :: (Marionette m) => Selector -> m Element
findElement selector = value <$> sendCommand Command{command = "WebDriver:FindElement", parameters = toJSON selector}

findElementFrom :: (Marionette m) => Element -> Selector -> m Element
findElementFrom element selector = value <$> sendCommand Command{command = "WebDriver:FindElement", parameters = toJSON (SelectorFrom element selector)}

-- findElementFromShadowRoot :: (Marionette m) => m ()
-- findElementFromShadowRoot = sendCommand_ Command{command = "WebDriver:FindElementFromShadowRoot", parameters = Aeson.object []}

findElements :: (Marionette m, IsList list, Item list ~ Element) => Selector -> m list
findElements selector = fromList <$> sendCommand Command{command = "WebDriver:FindElements", parameters = toJSON selector}

findElementsFrom :: (Marionette m, IsList list, Item list ~ Element) => Element -> Selector -> m list
findElementsFrom element selector = fromList <$> sendCommand Command{command = "WebDriver:FindElements", parameters = toJSON (SelectorFrom element selector)}

-- findElementsFromShadowRoot :: (Marionette m) => m ()
-- findElementsFromShadowRoot = sendCommand_ Command{command = "WebDriver:FindElementsFromShadowRoot", parameters = Aeson.object []}

forward :: (Marionette m) => m ()
forward = sendCommand_ Command{command = "WebDriver:Forward", parameters = Aeson.object []}

fullscreenWindow :: (Marionette m) => m ()
fullscreenWindow = sendCommand_ Command{command = "WebDriver:FullscreenWindow", parameters = Aeson.object []}

getActiveElement :: (Marionette m) => m ()
getActiveElement = sendCommand_ Command{command = "WebDriver:GetActiveElement", parameters = Aeson.object []}

getAlertText :: (Marionette m) => m ()
getAlertText = sendCommand_ Command{command = "WebDriver:GetAlertText", parameters = Aeson.object []}

getCapabilities :: (Marionette m) => m ()
getCapabilities = sendCommand_ Command{command = "WebDriver:GetCapabilities", parameters = Aeson.object []}

getComputedLabel :: (Marionette m) => m ()
getComputedLabel = sendCommand_ Command{command = "WebDriver:GetComputedLabel", parameters = Aeson.object []}

getComputedRole :: (Marionette m) => m ()
getComputedRole = sendCommand_ Command{command = "WebDriver:GetComputedRole", parameters = Aeson.object []}

getCookies :: (Marionette m) => m ()
getCookies = sendCommand_ Command{command = "WebDriver:GetCookies", parameters = Aeson.object []}

getCurrentURL :: (Marionette m) => m ()
getCurrentURL = sendCommand_ Command{command = "WebDriver:GetCurrentURL", parameters = Aeson.object []}

getElementAttribute :: (Marionette m) => Text -> Element -> m (Maybe Text)
getElementAttribute attr Element{..} =
    value
        <$> sendCommand
            Command
                { command = "WebDriver:GetElementAttribute"
                , parameters =
                    Aeson.object
                        [ "id" .= elementId
                        , "name" .= attr
                        ]
                }

getElementCSSValue :: (Marionette m) => m ()
getElementCSSValue = sendCommand_ Command{command = "WebDriver:GetElementCSSValue", parameters = Aeson.object []}

getElementProperty :: (Marionette m) => m ()
getElementProperty = sendCommand_ Command{command = "WebDriver:GetElementProperty", parameters = Aeson.object []}

data Rect = Rect
    { x :: Float
    , y :: Float
    , width :: Float
    , height :: Float
    }
    deriving stock (Generic, Show, Eq)
    deriving anyclass (FromJSON)

getElementRect :: (Marionette m) => Element -> m Rect
getElementRect Element{..} = sendCommand Command{command = "WebDriver:GetElementRect", parameters = Aeson.object ["id" .= elementId]}

getElementTagName :: (Marionette m) => m ()
getElementTagName = sendCommand_ Command{command = "WebDriver:GetElementTagName", parameters = Aeson.object []}

getElementText :: (Marionette m) => m ()
getElementText = sendCommand_ Command{command = "WebDriver:GetElementText", parameters = Aeson.object []}

getPageSource :: (Marionette m) => m ()
getPageSource = sendCommand_ Command{command = "WebDriver:GetPageSource", parameters = Aeson.object []}

getShadowRoot :: (Marionette m) => m ()
getShadowRoot = sendCommand_ Command{command = "WebDriver:GetShadowRoot", parameters = Aeson.object []}

getTimeouts :: (Marionette m) => m ()
getTimeouts = sendCommand_ Command{command = "WebDriver:GetTimeouts", parameters = Aeson.object []}

getTitle :: (Marionette m) => m ()
getTitle = sendCommand_ Command{command = "WebDriver:GetTitle", parameters = Aeson.object []}

getWindowHandle :: (Marionette m) => m ()
getWindowHandle = sendCommand_ Command{command = "WebDriver:GetWindowHandle", parameters = Aeson.object []}

getWindowHandles :: (Marionette m) => m ()
getWindowHandles = sendCommand_ Command{command = "WebDriver:GetWindowHandles", parameters = Aeson.object []}

getWindowRect :: (Marionette m) => m ()
getWindowRect = sendCommand_ Command{command = "WebDriver:GetWindowRect", parameters = Aeson.object []}

isElementDisplayed :: (Marionette m) => m ()
isElementDisplayed = sendCommand_ Command{command = "WebDriver:IsElementDisplayed", parameters = Aeson.object []}

isElementEnabled :: (Marionette m) => m ()
isElementEnabled = sendCommand_ Command{command = "WebDriver:IsElementEnabled", parameters = Aeson.object []}

isElementSelected :: (Marionette m) => m ()
isElementSelected = sendCommand_ Command{command = "WebDriver:IsElementSelected", parameters = Aeson.object []}

minimizeWindow :: (Marionette m) => m ()
minimizeWindow = sendCommand_ Command{command = "WebDriver:MinimizeWindow", parameters = Aeson.object []}

maximizeWindow :: (Marionette m) => m ()
maximizeWindow = sendCommand_ Command{command = "WebDriver:MaximizeWindow", parameters = Aeson.object []}

navigate :: (Marionette m) => Text -> m ()
navigate url =
    sendCommand_
        Command
            { command = "WebDriver:Navigate"
            , parameters = Aeson.object ["url" .= url]
            }

newSession :: (Marionette m) => m ()
newSession = sendCommand_ Command{command = "WebDriver:NewSession", parameters = Aeson.object []}

newWindow :: (Marionette m) => m ()
newWindow = sendCommand_ Command{command = "WebDriver:NewWindow", parameters = Aeson.object []}

performActions :: (Marionette m) => m ()
performActions = sendCommand_ Command{command = "WebDriver:PerformActions", parameters = Aeson.object []}

print :: (Marionette m) => m ()
print = sendCommand_ Command{command = "WebDriver:Print", parameters = Aeson.object []}

refresh :: (Marionette m) => m ()
refresh = sendCommand_ Command{command = "WebDriver:Refresh", parameters = Aeson.object []}

releaseActions :: (Marionette m) => m ()
releaseActions = sendCommand_ Command{command = "WebDriver:ReleaseActions", parameters = Aeson.object []}

sendAlertText :: (Marionette m) => m ()
sendAlertText = sendCommand_ Command{command = "WebDriver:SendAlertText", parameters = Aeson.object []}

setPermission :: (Marionette m) => m ()
setPermission = sendCommand_ Command{command = "WebDriver:SetPermission", parameters = Aeson.object []}

setTimeouts :: (Marionette m) => m ()
setTimeouts = sendCommand_ Command{command = "WebDriver:SetTimeouts", parameters = Aeson.object []}

setWindowRect :: (Marionette m) => m ()
setWindowRect = sendCommand_ Command{command = "WebDriver:SetWindowRect", parameters = Aeson.object []}

switchToFrame :: (Marionette m) => m ()
switchToFrame = sendCommand_ Command{command = "WebDriver:SwitchToFrame", parameters = Aeson.object []}

switchToParentFrame :: (Marionette m) => m ()
switchToParentFrame = sendCommand_ Command{command = "WebDriver:SwitchToParentFrame", parameters = Aeson.object []}

switchToWindow :: (Marionette m) => m ()
switchToWindow = sendCommand_ Command{command = "WebDriver:SwitchToWindow", parameters = Aeson.object []}

takeScreenshot :: (Marionette m, MonadThrow m) => m ByteString
takeScreenshot =
    either (throwM . AssertionFailed) (pure . Base64.decodeLenient)
        . Aeson.parseEither (Aeson.withText "value" $ pure . Text.encodeUtf8)
        =<< sendCommand
            Command
                { command = "WebDriver:TakeScreenshot"
                , parameters = Aeson.object []
                }

--   // WebAuthn
--   "WebAuthn:AddVirtualAuthenticator":
--     GeckoDriver.prototype.addVirtualAuthenticator,
--   "WebAuthn:RemoveVirtualAuthenticator":
--     GeckoDriver.prototype.removeVirtualAuthenticator,
--   "WebAuthn:AddCredential": GeckoDriver.prototype.addCredential,
--   "WebAuthn:GetCredentials": GeckoDriver.prototype.getCredentials,
--   "WebAuthn:RemoveCredential": GeckoDriver.prototype.removeCredential,
--   "WebAuthn:RemoveAllCredentials": GeckoDriver.prototype.removeAllCredentials,
--   "WebAuthn:SetUserVerified": GeckoDriver.prototype.setUserVerified,
