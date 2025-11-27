{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Effectful.Marionette
    ( module Effectful.Marionette
    , Element (..)
    , Rect (..)
    , Selector (..)
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
import Effectful.Concurrent.STM (Concurrent, TMVar, TQueue, atomically, newEmptyTMVarIO, readTMVar, writeTQueue)
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
import GHC.Stack (HasCallStack)
import Test.Marionette.Client
    ( CommandWithCallback (CommandWithCallback)
    , MarionetteClient (..)
    )
import Test.Marionette.Client qualified as Marionette
import Test.Marionette.Commands (Element (..), Rect (..), Selector (..))
import Test.Marionette.Commands qualified as Marionette
import Test.Marionette.Protocol qualified as Marionette
import Prelude

instance {-# OVERLAPPING #-} (Error Marionette.Error :> es) => MonadError Marionette.Error (Eff es) where
    throwError :: Marionette.Error -> Eff es a
    throwError = Error.throwError
    catchError :: Eff es a -> (Marionette.Error -> Eff es a) -> Eff es a
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
        either Error.throwError pure =<< Marionette.parseResult =<< atomically (readTMVar result)
    sendCommands = mapConcurrently sendCommand

data Marionette :: Effect

type role Marionette phantom phantom

type instance DispatchOf Marionette = 'Static 'WithSideEffects

newtype instance StaticRep Marionette = Marionette (TQueue CommandWithCallback)

runMarionette :: (IOE :> es) => Eff (Marionette ': es) a -> Eff es a
runMarionette action =
    withEffToIO (ConcUnlift Ephemeral (Limited 1)) \unlift -> Marionette.runMarionette do
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

quit :: (HasCallStack, Marionette :> es) => Eff es ()
quit = m Marionette.quit

-- data WebDriver :: Effect
--
-- type role WebDriver phantom phantom
--
-- type instance DispatchOf WebDriver = 'Static 'WithSideEffects
--
-- newtype instance StaticRep WebDriver = WebDriver WDSession
--
-- instance WebDriver.WDSessionState (Eff (WebDriver ': es)) where
--     getSession = coerce <$> getStaticRep @WebDriver
--     putSession = putStaticRep . WebDriver
--
-- instance (IOE :> es) => WebDriver.WebDriver (Eff (WebDriver ': es)) where
--     doCommand = ((wd .) .) . WebDriver.doCommand
--
-- runWebDriver :: (IOE :> es) => WDConfig -> Eff (WebDriver ': es) a -> Eff es a
-- runWebDriver conf effect = do
--     session <- liftIO $ WebDriver.runSession conf WebDriver.getSession
--     evalStaticRep (WebDriver session) $ WebDriver.finallyClose effect
--
-- wd :: (WebDriver :> es) => WD a -> Eff es a
-- wd wd = do
--     WebDriver session <- getStaticRep
--     unsafeEff_ $ WebDriver.runWD session wd

-- acceptAlert :: (HasCallStack, Marionette :> es) => Eff es ()
-- acceptAlert = sendCommand_ Command{command = "WebDriver:AcceptAlert", parameters = Aeson.object []}
--
-- acceptDialog :: (HasCallStack, Marionette :> es) => Eff es ()
-- acceptDialog = sendCommand_ Command{command = "WebDriver:AcceptDialog", parameters = Aeson.object []}
--
-- addCookie :: (HasCallStack, Marionette :> es) => Eff es ()
-- addCookie = sendCommand_ Command{command = "WebDriver:AddCookie", parameters = Aeson.object []}
--
-- back :: (HasCallStack, Marionette :> es) => Eff es ()
-- back = sendCommand_ Command{command = "WebDriver:Back", parameters = Aeson.object []}
--
-- closeChromeWindow :: (HasCallStack, Marionette :> es) => Eff es ()
-- closeChromeWindow = sendCommand_ Command{command = "WebDriver:CloseChromeWindow", parameters = Aeson.object []}
--
-- closeWindow :: (HasCallStack, Marionette :> es) => Eff es ()
-- closeWindow = sendCommand_ Command{command = "WebDriver:CloseWindow", parameters = Aeson.object []}
--
-- deleteAllCookies :: (HasCallStack, Marionette :> es) => Eff es ()
-- deleteAllCookies = sendCommand_ Command{command = "WebDriver:DeleteAllCookies", parameters = Aeson.object []}
--
-- deleteCookie :: (HasCallStack, Marionette :> es) => Eff es ()
-- deleteCookie = sendCommand_ Command{command = "WebDriver:DeleteCookie", parameters = Aeson.object []}
--
-- deleteSession :: (HasCallStack, Marionette :> es) => Eff es ()
-- deleteSession = sendCommand_ Command{command = "WebDriver:DeleteSession", parameters = Aeson.object []}
--
-- dismissAlert :: (HasCallStack, Marionette :> es) => Eff es ()
-- dismissAlert = sendCommand_ Command{command = "WebDriver:DismissAlert", parameters = Aeson.object []}
--
-- elementClear :: (HasCallStack, Marionette :> es) => Eff es ()
-- elementClear = sendCommand_ Command{command = "WebDriver:ElementClear", parameters = Aeson.object []}
--
elementClick :: (HasCallStack, Marionette :> es) => Element -> Eff es ()
elementClick = m . Marionette.elementClick

--
elementSendKeys :: (HasCallStack, Marionette :> es) => Element -> Text -> Eff es ()
elementSendKeys = (m .) . Marionette.elementSendKeys

--
executeAsyncScript :: (HasCallStack, Marionette :> es, Foldable f, FromJSON a) => Text -> f Value -> Eff es (Maybe a)
executeAsyncScript = (m .) . Marionette.executeAsyncScript

--
executeScript :: (HasCallStack, Marionette :> es, Foldable f, FromJSON a) => Text -> f Value -> Eff es a
executeScript = (m .) . Marionette.executeScript

--
findElement :: (HasCallStack, Marionette :> es) => Selector -> Eff es Element
findElement = m . Marionette.findElement

--
findElementFrom :: (HasCallStack, Marionette :> es) => Element -> Selector -> Eff es Element
findElementFrom = (m .) . Marionette.findElementFrom

--
-- -- findElementFromShadowRoot :: (HasCallStack, Marionette :> es) => Eff es ()
-- -- findElementFromShadowRoot = sendCommand_ Command{command = "WebDriver:FindElementFromShadowRoot", parameters = Aeson.object []}
--
-- findElements :: (HasCallStack, Marionette :> es, IsList list, Item list ~ Element) => Selector -> Eff es list
-- findElements selector = fromList <$> sendCommand Command{command = "WebDriver:FindElements", parameters = toJSON selector}
--
-- findElementsFrom :: (HasCallStack, Marionette :> es, IsList list, Item list ~ Element) => Element -> Selector -> Eff es list
-- findElementsFrom element selector = fromList <$> sendCommand Command{command = "WebDriver:FindElements", parameters = toJSON (SelectorFrom element selector)}
--
-- -- findElementsFromShadowRoot :: (HasCallStack, Marionette :> es) => Eff es ()
-- -- findElementsFromShadowRoot = sendCommand_ Command{command = "WebDriver:FindElementsFromShadowRoot", parameters = Aeson.object []}
--
-- forward :: (HasCallStack, Marionette :> es) => Eff es ()
-- forward = sendCommand_ Command{command = "WebDriver:Forward", parameters = Aeson.object []}
--
-- fullscreenWindow :: (HasCallStack, Marionette :> es) => Eff es ()
-- fullscreenWindow = sendCommand_ Command{command = "WebDriver:FullscreenWindow", parameters = Aeson.object []}
--
-- getActiveElement :: (HasCallStack, Marionette :> es) => Eff es ()
-- getActiveElement = sendCommand_ Command{command = "WebDriver:GetActiveElement", parameters = Aeson.object []}
--
-- getAlertText :: (HasCallStack, Marionette :> es) => Eff es ()
-- getAlertText = sendCommand_ Command{command = "WebDriver:GetAlertText", parameters = Aeson.object []}
--
-- getCapabilities :: (HasCallStack, Marionette :> es) => Eff es ()
-- getCapabilities = sendCommand_ Command{command = "WebDriver:GetCapabilities", parameters = Aeson.object []}
--
-- getComputedLabel :: (HasCallStack, Marionette :> es) => Eff es ()
-- getComputedLabel = sendCommand_ Command{command = "WebDriver:GetComputedLabel", parameters = Aeson.object []}
--
-- getComputedRole :: (HasCallStack, Marionette :> es) => Eff es ()
-- getComputedRole = sendCommand_ Command{command = "WebDriver:GetComputedRole", parameters = Aeson.object []}
--
-- getCookies :: (HasCallStack, Marionette :> es) => Eff es ()
-- getCookies = sendCommand_ Command{command = "WebDriver:GetCookies", parameters = Aeson.object []}
--
getCurrentURL :: (HasCallStack, Marionette :> es) => Eff es Text
getCurrentURL = m Marionette.getCurrentURL

-- getCurrentURL = sendCommand_ Command{command = "WebDriver:GetCurrentURL", parameters = Aeson.object []}
--
getElementAttribute :: (HasCallStack, Marionette :> es) => Text -> Element -> Eff es (Maybe Text)
getElementAttribute = (m .) . Marionette.getElementAttribute

--
-- getElementCSSValue :: (HasCallStack, Marionette :> es) => Eff es ()
-- getElementCSSValue = sendCommand_ Command{command = "WebDriver:GetElementCSSValue", parameters = Aeson.object []}
--
getElementProperty :: (HasCallStack, Marionette :> es) => Text -> Element -> Eff es (Maybe Text)
getElementProperty = (m .) . Marionette.getElementProperty
--
-- getElementRect :: (HasCallStack, Marionette :> es) => Element -> Eff es Rect
-- getElementRect Element{..} = sendCommand Command{command = "WebDriver:GetElementRect", parameters = Aeson.object ["id" .= elementId]}
--
-- getElementTagName :: (HasCallStack, Marionette :> es) => Eff es ()
-- getElementTagName = sendCommand_ Command{command = "WebDriver:GetElementTagName", parameters = Aeson.object []}
--
getElementText :: (HasCallStack, Marionette :> es) => Element -> Eff es Text
getElementText = m . Marionette.getElementText

--
-- getPageSource :: (HasCallStack, Marionette :> es) => Eff es ()
-- getPageSource = sendCommand_ Command{command = "WebDriver:GetPageSource", parameters = Aeson.object []}
--
-- getShadowRoot :: (HasCallStack, Marionette :> es) => Eff es ()
-- getShadowRoot = sendCommand_ Command{command = "WebDriver:GetShadowRoot", parameters = Aeson.object []}
--
-- getTimeouts :: (HasCallStack, Marionette :> es) => Eff es ()
-- getTimeouts = sendCommand_ Command{command = "WebDriver:GetTimeouts", parameters = Aeson.object []}
--
-- getTitle :: (HasCallStack, Marionette :> es) => Eff es ()
-- getTitle = sendCommand_ Command{command = "WebDriver:GetTitle", parameters = Aeson.object []}
--
-- getWindowHandle :: (HasCallStack, Marionette :> es) => Eff es ()
-- getWindowHandle = sendCommand_ Command{command = "WebDriver:GetWindowHandle", parameters = Aeson.object []}
--
-- getWindowHandles :: (HasCallStack, Marionette :> es) => Eff es ()
-- getWindowHandles = sendCommand_ Command{command = "WebDriver:GetWindowHandles", parameters = Aeson.object []}
--
-- getWindowRect :: (HasCallStack, Marionette :> es) => Eff es ()
-- getWindowRect = sendCommand_ Command{command = "WebDriver:GetWindowRect", parameters = Aeson.object []}
--
-- isElementDisplayed :: (HasCallStack, Marionette :> es) => Eff es ()
-- isElementDisplayed = sendCommand_ Command{command = "WebDriver:IsElementDisplayed", parameters = Aeson.object []}
--
-- isElementEnabled :: (HasCallStack, Marionette :> es) => Eff es ()
-- isElementEnabled = sendCommand_ Command{command = "WebDriver:IsElementEnabled", parameters = Aeson.object []}
--
-- isElementSelected :: (HasCallStack, Marionette :> es) => Eff es ()
-- isElementSelected = sendCommand_ Command{command = "WebDriver:IsElementSelected", parameters = Aeson.object []}
--
-- minimizeWindow :: (HasCallStack, Marionette :> es) => Eff es ()
-- minimizeWindow = sendCommand_ Command{command = "WebDriver:MinimizeWindow", parameters = Aeson.object []}
--
-- maximizeWindow :: (HasCallStack, Marionette :> es) => Eff es ()
-- maximizeWindow = sendCommand_ Command{command = "WebDriver:MaximizeWindow", parameters = Aeson.object []}
--
navigate :: (HasCallStack, Marionette :> es) => Text -> Eff es ()
navigate = m . Marionette.navigate

--
newSession :: (HasCallStack, Marionette :> es) => Eff es ()
newSession = m Marionette.newSession

--
-- newWindow :: (HasCallStack, Marionette :> es) => Eff es ()
-- newWindow = sendCommand_ Command{command = "WebDriver:NewWindow", parameters = Aeson.object []}
--
-- performActions :: (HasCallStack, Marionette :> es) => Eff es ()
-- performActions = sendCommand_ Command{command = "WebDriver:PerformActions", parameters = Aeson.object []}
--
-- print :: (HasCallStack, Marionette :> es) => Eff es ()
-- print = sendCommand_ Command{command = "WebDriver:Print", parameters = Aeson.object []}
--
refresh :: (HasCallStack, Marionette :> es) => Eff es ()
refresh = m Marionette.refresh

--
-- releaseActions :: (HasCallStack, Marionette :> es) => Eff es ()
-- releaseActions = sendCommand_ Command{command = "WebDriver:ReleaseActions", parameters = Aeson.object []}
--
-- sendAlertText :: (HasCallStack, Marionette :> es) => Eff es ()
-- sendAlertText = sendCommand_ Command{command = "WebDriver:SendAlertText", parameters = Aeson.object []}
--
-- setPermission :: (HasCallStack, Marionette :> es) => Eff es ()
-- setPermission = sendCommand_ Command{command = "WebDriver:SetPermission", parameters = Aeson.object []}
--
-- setTimeouts :: (HasCallStack, Marionette :> es) => Eff es ()
-- setTimeouts = sendCommand_ Command{command = "WebDriver:SetTimeouts", parameters = Aeson.object []}
--
-- setWindowRect :: (HasCallStack, Marionette :> es) => Eff es ()
-- setWindowRect = sendCommand_ Command{command = "WebDriver:SetWindowRect", parameters = Aeson.object []}
--
-- switchToFrame :: (HasCallStack, Marionette :> es) => Eff es ()
-- switchToFrame = sendCommand_ Command{command = "WebDriver:SwitchToFrame", parameters = Aeson.object []}
--
-- switchToParentFrame :: (HasCallStack, Marionette :> es) => Eff es ()
-- switchToParentFrame = sendCommand_ Command{command = "WebDriver:SwitchToParentFrame", parameters = Aeson.object []}
--
-- switchToWindow :: (HasCallStack, Marionette :> es) => Eff es ()
-- switchToWindow = sendCommand_ Command{command = "WebDriver:SwitchToWindow", parameters = Aeson.object []}
--
takeScreenshot :: (HasCallStack, Marionette :> es) => Eff es ByteString
takeScreenshot = m Marionette.takeScreenshot
