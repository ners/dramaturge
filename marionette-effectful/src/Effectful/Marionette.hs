{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Effectful.Marionette
    ( module Effectful.Marionette
    , AccessibilityProperties (..)
    , AuthenticatorId (..)
    , Context (..)
    , Cookie (..)
    , Credential (..)
    , CredentialId (..)
    , Element (..)
    , Frame (..)
    , NewWindowResult (..)
    , Orientation (..)
    , Rect (..)
    , RegistryEntry (..)
    , Selector (..)
    , Shadow (..)
    , Timeouts (..)
    , VirtualAuthenticator (..)
    , WindowHandle (..)
    )
where

import Control.Monad.Catch (MonadThrow (throwM))
import Control.Monad.Error.Class (MonadError (..))
import Data.Aeson (FromJSON, Value)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Effectful
    ( Dispatch (Static)
    , DispatchOf
    , Eff
    , Effect
    , IOE
    , Limit (Limited)
    , MonadIO (liftIO)
    , Persistence (Ephemeral)
    , UnliftStrategy (ConcUnlift)
    , inject
    , runEff
    , withEffToIO
    , (:>)
    )
import Effectful.Concurrent.Async (mapConcurrently, runConcurrent)
import Effectful.Concurrent.STM
    ( Concurrent
    , TMVar
    , TQueue
    , atomically
    , newEmptyTMVarIO
    , readTMVar
    , writeTQueue
    )
import Effectful.Dispatch.Static
    ( SideEffects (WithSideEffects)
    , StaticRep
    , evalStaticRep
    , getStaticRep
    , unsafeEff_
    )
import Effectful.Error.Static (Error, runError)
import Effectful.Error.Static qualified as Error
import Effectful.Reader.Static (Reader, runReader)
import Effectful.Reader.Static qualified as Reader
import GHC.IsList (IsList (..))
import GHC.Stack (HasCallStack)
import Test.Marionette
    ( AccessibilityProperties (..)
    , AuthenticatorId (..)
    , Context (..)
    , Cookie (..)
    , Credential (..)
    , CredentialId (..)
    , Element (..)
    , Frame (..)
    , NewWindowResult (..)
    , Orientation (..)
    , Rect (..)
    , RegistryEntry (..)
    , Selector (..)
    , Shadow (..)
    , Timeouts (..)
    , VirtualAuthenticator (..)
    , WindowHandle (..)
    )
import Test.Marionette qualified as Marionette
import Test.Marionette.Client
import Test.Marionette.Protocol qualified as Marionette
import Prelude

instance {-# OVERLAPPING #-} (Error Marionette.Error :> es) => MonadError Marionette.Error (Eff es) where
    throwError
        :: Marionette.Error
        -> Eff es a
    throwError = Error.throwError
    catchError
        :: Eff es a
        -> (Marionette.Error -> Eff es a)
        -> Eff es a
    catchError v = Error.catchError v . const

instance
    ( Concurrent :> es
    , Reader (TQueue CommandWithCallback) :> es
    , Error Marionette.Error :> es
    , IOE :> es
    )
    => MarionetteClient (Eff es)
    where
    sendCommand command = do
        q <- Reader.ask
        result :: TMVar Marionette.Result <- newEmptyTMVarIO
        atomically . writeTQueue q $ CommandWithCallback command result
        either Error.throwError pure
            =<< Marionette.parseResult
            =<< atomically (readTMVar result)
    sendCommands = mapConcurrently sendCommand

data Marionette :: Effect

type role Marionette phantom phantom

type instance DispatchOf Marionette = 'Static 'WithSideEffects

newtype instance StaticRep Marionette = Marionette (TQueue CommandWithCallback)

runMarionette :: (IOE :> es) => Eff (Marionette ': es) a -> Eff es a
runMarionette action =
    withEffToIO (ConcUnlift Ephemeral (Limited 1)) \unlift ->
        Marionette.runMarionette do
            sendQueue <- Marionette.getSendQueue
            liftIO . unlift . evalStaticRep (Marionette sendQueue) . inject $ action

m
    :: (HasCallStack, Marionette :> es')
    => Eff
        '[ Concurrent
         , Reader (TQueue CommandWithCallback)
         , Error Marionette.Error
         , IOE
         ]
        a
    -> Eff es' a
m action = do
    Marionette sendQueue <- getStaticRep
    either (throwM . snd) pure
        =<< ( unsafeEff_
                . runEff
                . runError
                . runReader sendQueue
                . runConcurrent
            )
            action

acceptConnections :: (HasCallStack, Marionette :> es) => Bool -> Eff es ()
acceptConnections = m . Marionette.acceptConnections

getAccessibilityPropertiesForAccessibilityNode
    :: (HasCallStack, Marionette :> es)
    => Text
    -> Eff es AccessibilityProperties
getAccessibilityPropertiesForAccessibilityNode = m . Marionette.getAccessibilityPropertiesForAccessibilityNode

getAccessibilityPropertiesForElement
    :: (HasCallStack, Marionette :> es)
    => Element
    -> Eff es AccessibilityProperties
getAccessibilityPropertiesForElement = m . Marionette.getAccessibilityPropertiesForElement

getContext :: (HasCallStack, Marionette :> es) => Eff es Context
getContext = m Marionette.getContext

getScreenOrientation :: (HasCallStack, Marionette :> es) => Eff es Orientation
getScreenOrientation = m Marionette.getScreenOrientation

getWindowType :: (HasCallStack, Marionette :> es) => Eff es Text
getWindowType = m Marionette.getWindowType

quit :: (HasCallStack, Marionette :> es) => Eff es ()
quit = m Marionette.quit

registerChromeHandler
    :: (HasCallStack, Marionette :> es)
    => Text
    -> [RegistryEntry]
    -> Eff es Text
registerChromeHandler = (m .) . Marionette.registerChromeHandler

setContext :: (HasCallStack, Marionette :> es) => Context -> Eff es ()
setContext = m . Marionette.setContext

setScreenOrientation
    :: (HasCallStack, Marionette :> es) => Orientation -> Eff es ()
setScreenOrientation = m . Marionette.setScreenOrientation

unregisterChromeHandler :: (HasCallStack, Marionette :> es) => Text -> Eff es ()
unregisterChromeHandler = m . Marionette.unregisterChromeHandler

acceptAlert :: (HasCallStack, Marionette :> es) => Eff es ()
acceptAlert = m Marionette.acceptAlert

addCookie :: (HasCallStack, Marionette :> es) => Cookie -> Eff es ()
addCookie = m . Marionette.addCookie

back :: (HasCallStack, Marionette :> es) => Eff es ()
back = m Marionette.back

closeChromeWindow :: (HasCallStack, Marionette :> es) => Eff es ()
closeChromeWindow = m Marionette.closeChromeWindow

closeWindow :: (HasCallStack, Marionette :> es) => Eff es ()
closeWindow = m Marionette.closeWindow

deleteAllCookies :: (HasCallStack, Marionette :> es) => Eff es ()
deleteAllCookies = m Marionette.deleteAllCookies

deleteCookie :: (HasCallStack, Marionette :> es) => Text -> Eff es ()
deleteCookie = m . Marionette.deleteCookie

deleteSession :: (HasCallStack, Marionette :> es) => Eff es ()
deleteSession = m Marionette.deleteSession

dismissAlert :: (HasCallStack, Marionette :> es) => Eff es ()
dismissAlert = m Marionette.dismissAlert

elementClear :: (HasCallStack, Marionette :> es) => Element -> Eff es ()
elementClear = m . Marionette.elementClear

elementClick :: (HasCallStack, Marionette :> es) => Element -> Eff es ()
elementClick = m . Marionette.elementClick

elementSendKeys
    :: (HasCallStack, Marionette :> es) => Element -> Text -> Eff es ()
elementSendKeys = (m .) . Marionette.elementSendKeys

executeAsyncScript
    :: (HasCallStack, Marionette :> es, Foldable f, FromJSON a)
    => Text
    -> f Value
    -> Eff es (Maybe a)
executeAsyncScript = (m .) . Marionette.executeAsyncScript

executeScript
    :: (HasCallStack, Marionette :> es, Foldable f, FromJSON a)
    => Text
    -> f Value
    -> Eff es a
executeScript = (m .) . Marionette.executeScript

findElement :: (HasCallStack, Marionette :> es) => Selector -> Eff es Element
findElement = m . Marionette.findElement

findElementFrom
    :: (HasCallStack, Marionette :> es)
    => Element
    -> Selector
    -> Eff es Element
findElementFrom = (m .) . Marionette.findElementFrom

findElementFromShadowRoot
    :: (HasCallStack, Marionette :> es)
    => Shadow
    -> Selector
    -> Eff es Element
findElementFromShadowRoot = (m .) . Marionette.findElementFromShadowRoot

findElements
    :: (HasCallStack, Marionette :> es, IsList list, Item list ~ Element)
    => Selector
    -> Eff es list
findElements = m . Marionette.findElements

findElementsFrom
    :: (HasCallStack, Marionette :> es, IsList list, Item list ~ Element)
    => Element
    -> Selector
    -> Eff es list
findElementsFrom = (m .) . Marionette.findElementsFrom

findElementsFromShadowRoot
    :: (HasCallStack, Marionette :> es, IsList list, Item list ~ Element)
    => Shadow
    -> Selector
    -> Eff es list
findElementsFromShadowRoot = (m .) . Marionette.findElementsFromShadowRoot

forward :: (HasCallStack, Marionette :> es) => Eff es ()
forward = m Marionette.forward

fullscreenWindow :: (HasCallStack, Marionette :> es) => Eff es ()
fullscreenWindow = m Marionette.fullscreenWindow

getActiveElement :: (HasCallStack, Marionette :> es) => Eff es Element
getActiveElement = m Marionette.getActiveElement

getAlertText :: (HasCallStack, Marionette :> es) => Eff es Text
getAlertText = m Marionette.getAlertText

getComputedLabel :: (HasCallStack, Marionette :> es) => Element -> Eff es Text
getComputedLabel = m . Marionette.getComputedLabel

getComputedRole :: (HasCallStack, Marionette :> es) => Element -> Eff es Text
getComputedRole = m . Marionette.getComputedRole

getCookies :: (HasCallStack, Marionette :> es) => Eff es [Cookie]
getCookies = m Marionette.getCookies

getCurrentURL :: (HasCallStack, Marionette :> es) => Eff es Text
getCurrentURL = m Marionette.getCurrentURL

getElementAttribute
    :: (HasCallStack, Marionette :> es)
    => Text
    -> Element
    -> Eff es (Maybe Text)
getElementAttribute = (m .) . Marionette.getElementAttribute

getElementCSSValue
    :: (HasCallStack, Marionette :> es)
    => Element
    -> Text
    -> Eff es (Maybe Text)
getElementCSSValue = (m .) . Marionette.getElementCSSValue

getElementProperty
    :: (HasCallStack, Marionette :> es)
    => Element
    -> Text
    -> Eff es (Maybe Text)
getElementProperty = (m .) . Marionette.getElementProperty

getElementRect :: (HasCallStack, Marionette :> es) => Element -> Eff es Rect
getElementRect = m . Marionette.getElementRect

getElementTagName :: (HasCallStack, Marionette :> es) => Element -> Eff es Text
getElementTagName = m . Marionette.getElementTagName

getElementText :: (HasCallStack, Marionette :> es) => Element -> Eff es Text
getElementText = m . Marionette.getElementText

getPageSource :: (HasCallStack, Marionette :> es) => Eff es Text
getPageSource = m Marionette.getPageSource

getShadowRoot :: (HasCallStack, Marionette :> es) => Element -> Eff es Shadow
getShadowRoot = m . Marionette.getShadowRoot

getTimeouts :: (HasCallStack, Marionette :> es) => Eff es Timeouts
getTimeouts = m Marionette.getTimeouts

getTitle :: (HasCallStack, Marionette :> es) => Eff es Text
getTitle = m Marionette.getTitle

getWindowHandle :: (HasCallStack, Marionette :> es) => Eff es WindowHandle
getWindowHandle = m Marionette.getWindowHandle

getWindowHandles :: (HasCallStack, Marionette :> es) => Eff es [WindowHandle]
getWindowHandles = m Marionette.getWindowHandles

getWindowRect :: (HasCallStack, Marionette :> es) => Eff es Rect
getWindowRect = m Marionette.getWindowRect

isElementDisplayed :: (HasCallStack, Marionette :> es) => Element -> Eff es Bool
isElementDisplayed = m . Marionette.isElementDisplayed

isElementEnabled :: (HasCallStack, Marionette :> es) => Element -> Eff es Bool
isElementEnabled = m . Marionette.isElementEnabled

isElementSelected :: (HasCallStack, Marionette :> es) => Element -> Eff es Bool
isElementSelected = m . Marionette.isElementSelected

maximizeWindow :: (HasCallStack, Marionette :> es) => Eff es ()
maximizeWindow = m Marionette.maximizeWindow

minimizeWindow :: (HasCallStack, Marionette :> es) => Eff es ()
minimizeWindow = m Marionette.minimizeWindow

navigate :: (HasCallStack, Marionette :> es) => Text -> Eff es ()
navigate = m . Marionette.navigate

newSession :: (HasCallStack, Marionette :> es) => Eff es ()
newSession = m Marionette.newSession

newWindow :: (HasCallStack, Marionette :> es) => Eff es NewWindowResult
newWindow = m Marionette.newWindow

newTab :: (HasCallStack, Marionette :> es) => Eff es NewWindowResult
newTab = m Marionette.newTab

performActions :: (HasCallStack, Marionette :> es) => Eff es ()
performActions = m Marionette.performActions

print :: (HasCallStack, Marionette :> es) => Eff es ()
print = m Marionette.print

refresh :: (HasCallStack, Marionette :> es) => Eff es ()
refresh = m Marionette.refresh

releaseActions :: (HasCallStack, Marionette :> es) => Eff es ()
releaseActions = m Marionette.releaseActions

sendAlertText :: (HasCallStack, Marionette :> es) => Text -> Eff es ()
sendAlertText = m . Marionette.sendAlertText

setPermission :: (HasCallStack, Marionette :> es) => Eff es ()
setPermission = m Marionette.setPermission

setTimeouts :: (HasCallStack, Marionette :> es) => Timeouts -> Eff es ()
setTimeouts = m . Marionette.setTimeouts

setWindowRect :: (HasCallStack, Marionette :> es) => Rect -> Eff es ()
setWindowRect = m . Marionette.setWindowRect

switchToFrame :: (HasCallStack, Marionette :> es) => Frame -> Eff es ()
switchToFrame = m . Marionette.switchToFrame

switchToParentFrame :: (HasCallStack, Marionette :> es) => Eff es ()
switchToParentFrame = m Marionette.switchToParentFrame

switchToWindow :: (HasCallStack, Marionette :> es) => WindowHandle -> Eff es ()
switchToWindow = m . Marionette.switchToWindow

takeScreenshot :: (HasCallStack, Marionette :> es) => Eff es ByteString
takeScreenshot = m Marionette.takeScreenshot

addCredential
    :: (HasCallStack, Marionette :> es)
    => AuthenticatorId
    -> Credential
    -> Eff es ()
addCredential = (m .) . Marionette.addCredential

addVirtualAuthenticator
    :: (HasCallStack, Marionette :> es)
    => VirtualAuthenticator
    -> Eff es AuthenticatorId
addVirtualAuthenticator = m . Marionette.addVirtualAuthenticator

getCredentials
    :: (HasCallStack, Marionette :> es)
    => AuthenticatorId
    -> Eff es [Credential]
getCredentials = m . Marionette.getCredentials

removeAllCredentials
    :: (HasCallStack, Marionette :> es)
    => AuthenticatorId
    -> Eff es ()
removeAllCredentials = m . Marionette.removeAllCredentials

removeCredential
    :: (HasCallStack, Marionette :> es)
    => AuthenticatorId
    -> CredentialId
    -> Eff es ()
removeCredential = (m .) . Marionette.removeCredential

removeVirtualAuthenticator
    :: (HasCallStack, Marionette :> es)
    => AuthenticatorId
    -> Eff es ()
removeVirtualAuthenticator = m . Marionette.removeVirtualAuthenticator

setUserVerified
    :: (HasCallStack, Marionette :> es)
    => AuthenticatorId
    -> Bool
    -> Eff es ()
setUserVerified = (m .) . Marionette.setUserVerified
