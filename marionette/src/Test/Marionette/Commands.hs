{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Marionette.Commands where

import Data.Aeson
    ( FromJSON
    , KeyValue (..)
    , ToJSON (toJSON)
    , Value (..)
    )
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as Object
import Data.ByteString (ByteString)
import Data.ByteString.Base64 qualified as Base64
import Data.Foldable qualified as Foldable
import Data.Text
import Data.Text.Encoding qualified as Text
import GHC.Generics (Generic)
import GHC.IsList (IsList (Item, fromList))
import GHC.Stack (HasCallStack)
import Test.Marionette.AccessibilityProperties (AccessibilityProperties)
import Test.Marionette.Class
import Test.Marionette.Client
import Test.Marionette.Context (Context)
import Test.Marionette.Cookie (Cookie)
import Test.Marionette.Element (Element (..), Shadow)
import Test.Marionette.Frame (Frame)
import Test.Marionette.Orientation (Orientation)
import Test.Marionette.Protocol
import Test.Marionette.Rect (Rect)
import Test.Marionette.Registry (RegistryEntry)
import Test.Marionette.Selector
    ( Selector
    , SelectorFrom (..)
    , SelectorFromShadowRoot (..)
    )
import Test.Marionette.Timeouts (Timeouts)
import Test.Marionette.WebAuthn
    ( AuthenticatorId (..)
    , Credential
    , CredentialId (..)
    , VirtualAuthenticator
    )
import Test.Marionette.Window
    ( NewWindowResult
    , WindowHandle (..)
    , WindowType (..)
    )
import Prelude hiding (log)

newtype ValueObject a = ValueObject {value :: a}
    deriving stock (Generic)
    deriving anyclass (FromJSON, ToJSON)

type role ValueObject representational

acceptConnections :: (HasCallStack, Marionette m) => Bool -> m ()
acceptConnections value =
    sendCommand_
        Command
            { command = "Marionette:AcceptConnections"
            , parameters = Aeson.object ["value" .= value]
            }

getAccessibilityPropertiesForAccessibilityNode
    :: (HasCallStack, Marionette m)
    => Text
    -> m AccessibilityProperties
getAccessibilityPropertiesForAccessibilityNode nodeId =
    sendCommand
        Command
            { command = "Marionette:GetAccessibilityPropertiesForAccessibilityNode"
            , parameters = Aeson.object ["nodeId" .= nodeId]
            }

getAccessibilityPropertiesForElement
    :: (HasCallStack, Marionette m)
    => Element
    -> m AccessibilityProperties
getAccessibilityPropertiesForElement Element{..} =
    sendCommand
        Command
            { command = "Marionette:GetAccessibilityPropertiesForElement"
            , parameters = Aeson.object ["id" .= elementId]
            }

getContext :: (HasCallStack, Marionette m) => m Context
getContext = value <$> sendCommand "Marionette:GetContext"

getScreenOrientation :: (HasCallStack, Marionette m) => m Orientation
getScreenOrientation = value <$> sendCommand "Marionette:GetScreenOrientation"

getWindowType :: (HasCallStack, Marionette m) => m Text
getWindowType = value <$> sendCommand "Marionette:GetWindowType"

quit :: (HasCallStack, Marionette m) => m ()
quit = sendCommand_ "Marionette:Quit"

registerChromeHandler
    :: (HasCallStack, Marionette m)
    => Text
    -> [RegistryEntry]
    -> m Text
registerChromeHandler manifestPath entries =
    sendCommand
        Command
            { command = "Marionette:RegisterChromeHandler"
            , parameters =
                Aeson.object
                    [ "manifestPath" .= manifestPath
                    , "entries" .= entries
                    ]
            }

setContext :: (HasCallStack, Marionette m) => Context -> m ()
setContext value =
    sendCommand_
        Command
            { command = "Marionette:SetContext"
            , parameters = Aeson.object ["value" .= value]
            }

setScreenOrientation :: (HasCallStack, Marionette m) => Orientation -> m ()
setScreenOrientation orientation =
    sendCommand_
        Command
            { command = "Marionette:SetScreenOrientation"
            , parameters = Aeson.object ["orientation" .= orientation]
            }

unregisterChromeHandler :: (HasCallStack, Marionette m) => Text -> m ()
unregisterChromeHandler handlerId =
    sendCommand_
        Command
            { command = "Marionette:UnregisterChromeHandler"
            , parameters = Aeson.object ["id" .= handlerId]
            }

acceptAlert :: (HasCallStack, Marionette m) => m ()
acceptAlert = sendCommand_ "WebDriver:AcceptAlert"

addCookie :: (HasCallStack, Marionette m) => Cookie -> m ()
addCookie cookie =
    sendCommand_
        Command
            { command = "WebDriver:AddCookie"
            , parameters = Aeson.object ["cookie" .= cookie]
            }

back :: (HasCallStack, Marionette m) => m ()
back = sendCommand_ "WebDriver:Back"

closeChromeWindow :: (HasCallStack, Marionette m) => m ()
closeChromeWindow = sendCommand_ "WebDriver:CloseChromeWindow"

closeWindow :: (HasCallStack, Marionette m) => m ()
closeWindow = sendCommand_ "WebDriver:CloseWindow"

deleteAllCookies :: (HasCallStack, Marionette m) => m ()
deleteAllCookies = sendCommand_ "WebDriver:DeleteAllCookies"

deleteCookie :: (HasCallStack, Marionette m) => Text -> m ()
deleteCookie name =
    sendCommand_
        Command
            { command = "WebDriver:DeleteCookie"
            , parameters = Aeson.object ["name" .= name]
            }

deleteSession :: (HasCallStack, Marionette m) => m ()
deleteSession = sendCommand_ "WebDriver:DeleteSession"

dismissAlert :: (HasCallStack, Marionette m) => m ()
dismissAlert = sendCommand_ "WebDriver:DismissAlert"

elementClear :: (HasCallStack, Marionette m) => Element -> m ()
elementClear Element{..} =
    sendCommand_
        Command
            { command = "WebDriver:ElementClear"
            , parameters = Aeson.object ["id" .= elementId]
            }

elementClick :: (HasCallStack, Marionette m) => Element -> m ()
elementClick Element{..} =
    sendCommand_
        Command
            { command = "WebDriver:ElementClick"
            , parameters = Aeson.object ["id" .= elementId]
            }

elementSendKeys :: (HasCallStack, Marionette m) => Element -> Text -> m ()
elementSendKeys Element{..} text =
    sendCommand_
        Command
            { command = "WebDriver:ElementSendKeys"
            , parameters =
                Aeson.object
                    [ "id" .= elementId
                    , "text" .= text
                    ]
            }

executeAsyncScript
    :: (HasCallStack, Marionette m, Foldable f, FromJSON a)
    => Text
    -> f Value
    -> m (Maybe a)
executeAsyncScript script args =
    fmap value . sendCommand $
        Command
            { command = "WebDriver:ExecuteAsyncScript"
            , parameters =
                Aeson.object
                    [ "script" .= script
                    , "args" .= Foldable.toList args
                    ]
            }

executeScript
    :: (HasCallStack, Marionette m, Foldable f, FromJSON a)
    => Text
    -> f Value
    -> m a
executeScript script args =
    fmap value . sendCommand $
        Command
            { command = "WebDriver:ExecuteScript"
            , parameters =
                Aeson.object
                    [ "script" .= script
                    , "args" .= Foldable.toList args
                    ]
            }

findElement :: (HasCallStack, Marionette m) => Selector -> m Element
findElement selector =
    value
        <$> sendCommand
            Command
                { command = "WebDriver:FindElement"
                , parameters = toJSON selector
                }

findElementFrom
    :: (HasCallStack, Marionette m)
    => Element
    -> Selector
    -> m Element
findElementFrom element selector =
    value
        <$> sendCommand
            Command
                { command = "WebDriver:FindElement"
                , parameters = toJSON $ SelectorFrom element selector
                }

findElementFromShadowRoot
    :: (HasCallStack, Marionette m)
    => Shadow
    -> Selector
    -> m Element
findElementFromShadowRoot shadowRoot selector =
    value
        <$> sendCommand
            Command
                { command = "WebDriver:FindElementFromShadowRoot"
                , parameters = toJSON $ SelectorFromShadowRoot shadowRoot selector
                }

findElements
    :: (HasCallStack, Marionette m, IsList list, Item list ~ Element)
    => Selector
    -> m list
findElements selector =
    fromList
        <$> sendCommand
            Command
                { command = "WebDriver:FindElements"
                , parameters = toJSON selector
                }

findElementsFrom
    :: (HasCallStack, Marionette m, IsList list, Item list ~ Element)
    => Element
    -> Selector
    -> m list
findElementsFrom element selector =
    fromList
        <$> sendCommand
            Command
                { command = "WebDriver:FindElements"
                , parameters = toJSON (SelectorFrom element selector)
                }

findElementsFromShadowRoot
    :: (HasCallStack, Marionette m, IsList list, Item list ~ Element)
    => Shadow
    -> Selector
    -> m list
findElementsFromShadowRoot shadowRoot selector =
    fromList
        <$> sendCommand
            Command
                { command = "WebDriver:FindElementsFromShadowRoot"
                , parameters = toJSON $ SelectorFromShadowRoot shadowRoot selector
                }

forward :: (HasCallStack, Marionette m) => m ()
forward = sendCommand_ "WebDriver:Forward"

fullscreenWindow :: (HasCallStack, Marionette m) => m ()
fullscreenWindow = sendCommand_ "WebDriver:FullscreenWindow"

getActiveElement :: (HasCallStack, Marionette m) => m Element
getActiveElement = value <$> sendCommand "WebDriver:GetActiveElement"

getAlertText :: (HasCallStack, Marionette m) => m Text
getAlertText = value <$> sendCommand "WebDriver:GetAlertText"

getComputedLabel :: (HasCallStack, Marionette m) => Element -> m Text
getComputedLabel Element{..} =
    value
        <$> sendCommand
            Command
                { command = "WebDriver:GetComputedLabel"
                , parameters = Aeson.object ["id" .= elementId]
                }

getComputedRole :: (HasCallStack, Marionette m) => Element -> m Text
getComputedRole Element{..} =
    value
        <$> sendCommand
            Command
                { command = "WebDriver:GetComputedRole"
                , parameters = Aeson.object ["id" .= elementId]
                }

getCookies :: (HasCallStack, Marionette m) => m [Cookie]
getCookies = sendCommand "WebDriver:GetCookies"

getCurrentURL :: (HasCallStack, Marionette m) => m Text
getCurrentURL = value <$> sendCommand "WebDriver:GetCurrentURL"

getElementAttribute
    :: (HasCallStack, Marionette m)
    => Text
    -> Element
    -> m (Maybe Text)
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

getElementCSSValue
    :: (HasCallStack, Marionette m)
    => Element
    -> Text
    -> m (Maybe Text)
getElementCSSValue Element{..} propertyName =
    value
        <$> sendCommand
            Command
                { command = "WebDriver:GetElementCSSValue"
                , parameters =
                    Aeson.object
                        [ "id" .= elementId
                        , "propertyName" .= propertyName
                        ]
                }

getElementProperty
    :: (HasCallStack, Marionette m)
    => Element
    -> Text
    -> m (Maybe Text)
getElementProperty Element{..} name =
    value
        <$> sendCommand
            Command
                { command = "WebDriver:GetElementProperty"
                , parameters =
                    Aeson.object
                        [ "id" .= elementId
                        , "name" .= name
                        ]
                }

getElementRect :: (HasCallStack, Marionette m) => Element -> m Rect
getElementRect Element{..} =
    sendCommand
        Command
            { command = "WebDriver:GetElementRect"
            , parameters = Aeson.object ["id" .= elementId]
            }

getElementTagName :: (HasCallStack, Marionette m) => Element -> m Text
getElementTagName Element{..} =
    value
        <$> sendCommand
            Command
                { command = "WebDriver:GetElementTagName"
                , parameters = Aeson.object ["id" .= elementId]
                }

getElementText :: (HasCallStack, Marionette m) => Element -> m Text
getElementText Element{..} =
    value
        <$> sendCommand
            Command
                { command = "WebDriver:GetElementText"
                , parameters = Aeson.object ["id" .= elementId]
                }

getPageSource :: (HasCallStack, Marionette m) => m Text
getPageSource = value <$> sendCommand "WebDriver:GetPageSource"

getShadowRoot :: (HasCallStack, Marionette m) => Element -> m Shadow
getShadowRoot Element{..} =
    value
        <$> sendCommand
            Command
                { command = "WebDriver:GetShadowRoot"
                , parameters = Aeson.object ["id" .= elementId]
                }

getTimeouts :: (HasCallStack, Marionette m) => m Timeouts
getTimeouts = sendCommand "WebDriver:GetTimeouts"

getTitle :: (HasCallStack, Marionette m) => m Text
getTitle = value <$> sendCommand "WebDriver:GetTitle"

getWindowHandle :: (HasCallStack, Marionette m) => m WindowHandle
getWindowHandle = value <$> sendCommand "WebDriver:GetWindowHandle"

getWindowHandles :: (HasCallStack, Marionette m) => m [WindowHandle]
getWindowHandles = sendCommand "WebDriver:GetWindowHandles"

getWindowRect :: (HasCallStack, Marionette m) => m Rect
getWindowRect = sendCommand "WebDriver:GetWindowRect"

isElementDisplayed :: (HasCallStack, Marionette m) => Element -> m Bool
isElementDisplayed Element{..} =
    value
        <$> sendCommand
            Command
                { command = "WebDriver:IsElementDisplayed"
                , parameters = Aeson.object ["id" .= elementId]
                }

isElementEnabled :: (HasCallStack, Marionette m) => Element -> m Bool
isElementEnabled Element{..} =
    value
        <$> sendCommand
            Command
                { command = "WebDriver:IsElementEnabled"
                , parameters = Aeson.object ["id" .= elementId]
                }

isElementSelected :: (HasCallStack, Marionette m) => Element -> m Bool
isElementSelected Element{..} =
    value
        <$> sendCommand
            Command
                { command = "WebDriver:IsElementSelected"
                , parameters = Aeson.object ["id" .= elementId]
                }

maximizeWindow :: (HasCallStack, Marionette m) => m ()
maximizeWindow = sendCommand_ "WebDriver:MaximizeWindow"

minimizeWindow :: (HasCallStack, Marionette m) => m ()
minimizeWindow = sendCommand_ "WebDriver:MinimizeWindow"

navigate :: (HasCallStack, Marionette m) => Text -> m ()
navigate url =
    sendCommand_
        Command
            { command = "WebDriver:Navigate"
            , parameters = Aeson.object ["url" .= url]
            }

newSession :: (HasCallStack, Marionette m) => m ()
newSession = sendCommand_ "WebDriver:NewSession"

newWindow :: (HasCallStack, Marionette m) => m NewWindowResult
newWindow =
    sendCommand
        Command
            { command = "WebDriver:NewWindow"
            , parameters = Aeson.object ["type" .= Window]
            }

newTab :: (HasCallStack, Marionette m) => m NewWindowResult
newTab =
    sendCommand
        Command
            { command = "WebDriver:NewWindow"
            , parameters = Aeson.object ["type" .= Tab]
            }

performActions :: (HasCallStack, Marionette m) => m ()
performActions = sendCommand_ "WebDriver:PerformActions"

print :: (HasCallStack, Marionette m) => m ()
print = sendCommand_ "WebDriver:Print"

refresh :: (HasCallStack, Marionette m) => m ()
refresh = sendCommand_ "WebDriver:Refresh"

releaseActions :: (HasCallStack, Marionette m) => m ()
releaseActions = sendCommand_ "WebDriver:ReleaseActions"

sendAlertText :: (HasCallStack, Marionette m) => Text -> m ()
sendAlertText text =
    sendCommand_
        Command
            { command = "WebDriver:SendAlertText"
            , parameters = Aeson.object ["text" .= text]
            }

setPermission :: (HasCallStack, Marionette m) => m ()
setPermission = sendCommand_ "WebDriver:SetPermission"

setTimeouts :: (HasCallStack, Marionette m) => Timeouts -> m ()
setTimeouts timeouts =
    sendCommand_
        Command
            { command = "WebDriver:SetTimeouts"
            , parameters = toJSON timeouts
            }

setWindowRect :: (HasCallStack, Marionette m) => Rect -> m ()
setWindowRect rect =
    sendCommand_
        Command
            { command = "WebDriver:SetWindowRect"
            , parameters = toJSON rect
            }

switchToFrame :: (HasCallStack, Marionette m) => Frame -> m ()
switchToFrame frame =
    sendCommand_
        Command
            { command = "WebDriver:SwitchToFrame"
            , parameters = toJSON frame
            }

switchToParentFrame :: (HasCallStack, Marionette m) => m ()
switchToParentFrame = sendCommand_ "WebDriver:SwitchToParentFrame"

switchToWindow :: (HasCallStack, Marionette m) => WindowHandle -> m ()
switchToWindow window =
    sendCommand_
        Command
            { command = "WebDriver:SwitchToWindow"
            , parameters = toJSON window
            }

takeScreenshot :: (HasCallStack, Marionette m) => m ByteString
takeScreenshot =
    Base64.decodeLenient
        . Text.encodeUtf8
        . value
        <$> sendCommand
            Command
                { command = "WebDriver:TakeScreenshot"
                , parameters = Aeson.object []
                }

addCredential
    :: (HasCallStack, Marionette m)
    => AuthenticatorId
    -> Credential
    -> m ()
addCredential authenticatorId credential =
    sendCommand_
        Command
            { command = "WebAuthn:AddCredential"
            , parameters =
                Aeson.Object . mconcat $
                    [ Object.fromList ["authenticatorId" .= authenticatorId]
                    , case toJSON credential of
                        Aeson.Object o -> o
                        _ -> mempty
                    ]
            }

addVirtualAuthenticator
    :: (HasCallStack, Marionette m)
    => VirtualAuthenticator
    -> m AuthenticatorId
addVirtualAuthenticator authenticator =
    value
        <$> sendCommand
            Command
                { command = "WebAuthn:AddVirtualAuthenticator"
                , parameters = toJSON authenticator
                }

getCredentials
    :: (HasCallStack, Marionette m)
    => AuthenticatorId
    -> m [Credential]
getCredentials authenticatorId =
    value
        <$> sendCommand
            Command
                { command = "WebAuthn:GetCredentials"
                , parameters = Aeson.object ["authenticatorId" .= authenticatorId]
                }

removeAllCredentials
    :: (HasCallStack, Marionette m)
    => AuthenticatorId
    -> m ()
removeAllCredentials authenticatorId =
    sendCommand_
        Command
            { command = "WebAuthn:RemoveAllCredentials"
            , parameters = Aeson.object ["authenticatorId" .= authenticatorId]
            }

removeCredential
    :: (HasCallStack, Marionette m)
    => AuthenticatorId
    -> CredentialId
    -> m ()
removeCredential authenticatorId credentialId =
    sendCommand_
        Command
            { command = "WebAuthn:RemoveCredential"
            , parameters =
                Aeson.object
                    [ "authenticatorId" .= authenticatorId
                    , "credentialId" .= credentialId
                    ]
            }

removeVirtualAuthenticator
    :: (HasCallStack, Marionette m)
    => AuthenticatorId
    -> m ()
removeVirtualAuthenticator authenticatorId =
    sendCommand_
        Command
            { command = "WebAuthn:RemoveVirtualAuthenticator"
            , parameters = Aeson.object ["authenticatorId" .= authenticatorId]
            }

setUserVerified
    :: (HasCallStack, Marionette m)
    => AuthenticatorId
    -> Bool
    -> m ()
setUserVerified authenticatorId verified =
    sendCommand_
        Command
            { command = "WebAuthn:SetUserVerified"
            , parameters =
                Aeson.object
                    [ "authenticatorId" .= authenticatorId
                    , "isUserVerified" .= verified
                    ]
            }
