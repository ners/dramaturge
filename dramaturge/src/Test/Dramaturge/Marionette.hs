{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Dramaturge.Marionette
    ( module Test.Dramaturge.Marionette
    , module Test.Marionette.Commands
    )
where

import Data.Text (Text)
import Effectful
    ( Dispatch (Static)
    , DispatchOf
    , Eff
    , Effect
    , IOE
    , Limit (Limited, Unlimited)
    , MonadIO (liftIO)
    , Persistence (Ephemeral, Persistent)
    , UnliftStrategy (ConcUnlift)
    , inject
    , runEff
    , withEffToIO
    , (:>)
    )
import Effectful.Concurrent.Async (mapConcurrently, runConcurrent)
import Effectful.Concurrent.STM (Concurrent, TMVar, TQueue, atomically, newEmptyTMVarIO, putTMVar, readTMVar, writeTQueue)
import Effectful.Dispatch.Static
    ( SideEffects (WithSideEffects)
    , StaticRep
    , evalStaticRep
    , getStaticRep
    , unsafeEff_
    )
import Effectful.Error.Static (Error, runError, throwError)
import Effectful.Reader.Static (Reader, runReader)
import Effectful.Reader.Static qualified as Reader
import Test.Marionette.Client
    ( CommandWithCallback (CommandWithCallback)
    , MarionetteClient (..)
    , MarionetteT (..)
    )
import Test.Marionette.Client qualified as Marionette
import Test.Marionette.Commands (Element (..), Rect (..), Selector (..))
import Test.Marionette.Commands qualified as Marionette
import Test.Marionette.Protocol qualified as Marionette
import Prelude

baz :: (IOE :> es) => CommandWithCallback (Eff es) -> Eff es (CommandWithCallback (MarionetteT IO))
baz (CommandWithCallback command callback) =
    CommandWithCallback command
        <$> withEffToIO
            (ConcUnlift Persistent Unlimited)
            \unlift -> pure $ liftIO . unlift . callback

instance
    ( Concurrent :> es
    , Reader (TQueue (CommandWithCallback (MarionetteT IO))) :> es
    , Error Marionette.Error :> es
    , IOE :> es
    )
    => MarionetteClient (Eff es)
    where
    sendCommand' command = do
        q <- Reader.ask
        atomically . writeTQueue q =<< baz command
    sendCommand command = do
        result :: TMVar Marionette.Result <- newEmptyTMVarIO
        sendCommand' . CommandWithCallback command $ atomically . putTMVar result
        either throwError pure . Marionette.parseResult =<< atomically (readTMVar result)
    sendCommands = mapConcurrently sendCommand

data Marionette :: Effect

type role Marionette phantom phantom

type instance DispatchOf Marionette = 'Static 'WithSideEffects

newtype instance StaticRep Marionette = Marionette (TQueue (CommandWithCallback (MarionetteT IO)))

runMarionette :: (IOE :> es) => Eff (Marionette ': es) a -> Eff es a
runMarionette action =
    withEffToIO (ConcUnlift Ephemeral (Limited 1)) \unlift -> Marionette.runMarionette do
        sendQueue <- Marionette.getSendQueue
        liftIO . unlift . evalStaticRep (Marionette sendQueue) . inject $ action

m
    :: (Marionette :> es')
    => Eff
        '[ Concurrent
         , Reader (TQueue (CommandWithCallback (MarionetteT IO)))
         , Error Marionette.Error
         , IOE
         ]
        a
    -> Eff es' a
m action = do
    Marionette sendQueue <- getStaticRep
    either undefined pure
        =<< ( unsafeEff_
                . runEff
                . runError
                . runReader sendQueue
                . runConcurrent
            )
            action

quit :: (Marionette :> es) => Eff es ()
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

-- acceptAlert :: (Marionette :> es) => Eff es ()
-- acceptAlert = sendCommand_ Command{command = "WebDriver:AcceptAlert", parameters = Aeson.object []}
--
-- acceptDialog :: (Marionette :> es) => Eff es ()
-- acceptDialog = sendCommand_ Command{command = "WebDriver:AcceptDialog", parameters = Aeson.object []}
--
-- addCookie :: (Marionette :> es) => Eff es ()
-- addCookie = sendCommand_ Command{command = "WebDriver:AddCookie", parameters = Aeson.object []}
--
-- back :: (Marionette :> es) => Eff es ()
-- back = sendCommand_ Command{command = "WebDriver:Back", parameters = Aeson.object []}
--
-- closeChromeWindow :: (Marionette :> es) => Eff es ()
-- closeChromeWindow = sendCommand_ Command{command = "WebDriver:CloseChromeWindow", parameters = Aeson.object []}
--
-- closeWindow :: (Marionette :> es) => Eff es ()
-- closeWindow = sendCommand_ Command{command = "WebDriver:CloseWindow", parameters = Aeson.object []}
--
-- deleteAllCookies :: (Marionette :> es) => Eff es ()
-- deleteAllCookies = sendCommand_ Command{command = "WebDriver:DeleteAllCookies", parameters = Aeson.object []}
--
-- deleteCookie :: (Marionette :> es) => Eff es ()
-- deleteCookie = sendCommand_ Command{command = "WebDriver:DeleteCookie", parameters = Aeson.object []}
--
-- deleteSession :: (Marionette :> es) => Eff es ()
-- deleteSession = sendCommand_ Command{command = "WebDriver:DeleteSession", parameters = Aeson.object []}
--
-- dismissAlert :: (Marionette :> es) => Eff es ()
-- dismissAlert = sendCommand_ Command{command = "WebDriver:DismissAlert", parameters = Aeson.object []}
--
-- elementClear :: (Marionette :> es) => Eff es ()
-- elementClear = sendCommand_ Command{command = "WebDriver:ElementClear", parameters = Aeson.object []}
--
-- elementClick :: (Marionette :> es) => Element -> Eff es ()
-- elementClick Element{..} =
--     sendCommand_
--         Command
--             { command = "WebDriver:ElementClick"
--             , parameters = Aeson.object ["id" .= elementId]
--             }
--
-- elementSendKeys :: (Marionette :> es) => Element -> Text -> Eff es ()
-- elementSendKeys Element{..} text =
--     sendCommand_
--         Command
--             { command = "WebDriver:ElementSendKeys"
--             , parameters = Aeson.object ["id" .= elementId, "text" .= text]
--             }
--
-- executeAsyncScript :: (Marionette :> es) => Eff es ()
-- executeAsyncScript = sendCommand_ Command{command = "WebDriver:ExecuteAsyncScript", parameters = Aeson.object []}
--
-- executeScript :: (Marionette :> es, Foldable f, FromJSON a) => Text -> f Value -> Eff es a
-- executeScript script args =
--     value
--         <$> sendCommand
--             Command
--                 { command = "WebDriver:ExecuteScript"
--                 , parameters = Aeson.object ["script" .= script, "args" .= Foldable.toList args]
--                 }
--
findElement :: (Marionette :> es) => Selector -> Eff es Element
findElement = m . Marionette.findElement
--
-- findElementFrom :: (Marionette :> es) => Element -> Selector -> Eff es Element
-- findElementFrom element selector = value <$> sendCommand Command{command = "WebDriver:FindElement", parameters = toJSON (SelectorFrom element selector)}
--
-- -- findElementFromShadowRoot :: (Marionette :> es) => Eff es ()
-- -- findElementFromShadowRoot = sendCommand_ Command{command = "WebDriver:FindElementFromShadowRoot", parameters = Aeson.object []}
--
-- findElements :: (Marionette :> es, IsList list, Item list ~ Element) => Selector -> Eff es list
-- findElements selector = fromList <$> sendCommand Command{command = "WebDriver:FindElements", parameters = toJSON selector}
--
-- findElementsFrom :: (Marionette :> es, IsList list, Item list ~ Element) => Element -> Selector -> Eff es list
-- findElementsFrom element selector = fromList <$> sendCommand Command{command = "WebDriver:FindElements", parameters = toJSON (SelectorFrom element selector)}
--
-- -- findElementsFromShadowRoot :: (Marionette :> es) => Eff es ()
-- -- findElementsFromShadowRoot = sendCommand_ Command{command = "WebDriver:FindElementsFromShadowRoot", parameters = Aeson.object []}
--
-- forward :: (Marionette :> es) => Eff es ()
-- forward = sendCommand_ Command{command = "WebDriver:Forward", parameters = Aeson.object []}
--
-- fullscreenWindow :: (Marionette :> es) => Eff es ()
-- fullscreenWindow = sendCommand_ Command{command = "WebDriver:FullscreenWindow", parameters = Aeson.object []}
--
-- getActiveElement :: (Marionette :> es) => Eff es ()
-- getActiveElement = sendCommand_ Command{command = "WebDriver:GetActiveElement", parameters = Aeson.object []}
--
-- getAlertText :: (Marionette :> es) => Eff es ()
-- getAlertText = sendCommand_ Command{command = "WebDriver:GetAlertText", parameters = Aeson.object []}
--
-- getCapabilities :: (Marionette :> es) => Eff es ()
-- getCapabilities = sendCommand_ Command{command = "WebDriver:GetCapabilities", parameters = Aeson.object []}
--
-- getComputedLabel :: (Marionette :> es) => Eff es ()
-- getComputedLabel = sendCommand_ Command{command = "WebDriver:GetComputedLabel", parameters = Aeson.object []}
--
-- getComputedRole :: (Marionette :> es) => Eff es ()
-- getComputedRole = sendCommand_ Command{command = "WebDriver:GetComputedRole", parameters = Aeson.object []}
--
-- getCookies :: (Marionette :> es) => Eff es ()
-- getCookies = sendCommand_ Command{command = "WebDriver:GetCookies", parameters = Aeson.object []}
--
-- getCurrentURL :: (Marionette :> es) => Eff es ()
-- getCurrentURL = sendCommand_ Command{command = "WebDriver:GetCurrentURL", parameters = Aeson.object []}
--
-- getElementAttribute :: (Marionette :> es) => Text -> Element -> Eff es (Maybe Text)
-- getElementAttribute attr Element{..} =
--     value
--         <$> sendCommand
--             Command
--                 { command = "WebDriver:GetElementAttribute"
--                 , parameters =
--                     Aeson.object
--                         [ "id" .= elementId
--                         , "name" .= attr
--                         ]
--                 }
--
-- getElementCSSValue :: (Marionette :> es) => Eff es ()
-- getElementCSSValue = sendCommand_ Command{command = "WebDriver:GetElementCSSValue", parameters = Aeson.object []}
--
-- getElementProperty :: (Marionette :> es) => Eff es ()
-- getElementProperty = sendCommand_ Command{command = "WebDriver:GetElementProperty", parameters = Aeson.object []}
--
-- getElementRect :: (Marionette :> es) => Element -> Eff es Rect
-- getElementRect Element{..} = sendCommand Command{command = "WebDriver:GetElementRect", parameters = Aeson.object ["id" .= elementId]}
--
-- getElementTagName :: (Marionette :> es) => Eff es ()
-- getElementTagName = sendCommand_ Command{command = "WebDriver:GetElementTagName", parameters = Aeson.object []}
--
-- getElementText :: (Marionette :> es) => Eff es ()
-- getElementText = sendCommand_ Command{command = "WebDriver:GetElementText", parameters = Aeson.object []}
--
-- getPageSource :: (Marionette :> es) => Eff es ()
-- getPageSource = sendCommand_ Command{command = "WebDriver:GetPageSource", parameters = Aeson.object []}
--
-- getShadowRoot :: (Marionette :> es) => Eff es ()
-- getShadowRoot = sendCommand_ Command{command = "WebDriver:GetShadowRoot", parameters = Aeson.object []}
--
-- getTimeouts :: (Marionette :> es) => Eff es ()
-- getTimeouts = sendCommand_ Command{command = "WebDriver:GetTimeouts", parameters = Aeson.object []}
--
-- getTitle :: (Marionette :> es) => Eff es ()
-- getTitle = sendCommand_ Command{command = "WebDriver:GetTitle", parameters = Aeson.object []}
--
-- getWindowHandle :: (Marionette :> es) => Eff es ()
-- getWindowHandle = sendCommand_ Command{command = "WebDriver:GetWindowHandle", parameters = Aeson.object []}
--
-- getWindowHandles :: (Marionette :> es) => Eff es ()
-- getWindowHandles = sendCommand_ Command{command = "WebDriver:GetWindowHandles", parameters = Aeson.object []}
--
-- getWindowRect :: (Marionette :> es) => Eff es ()
-- getWindowRect = sendCommand_ Command{command = "WebDriver:GetWindowRect", parameters = Aeson.object []}
--
-- isElementDisplayed :: (Marionette :> es) => Eff es ()
-- isElementDisplayed = sendCommand_ Command{command = "WebDriver:IsElementDisplayed", parameters = Aeson.object []}
--
-- isElementEnabled :: (Marionette :> es) => Eff es ()
-- isElementEnabled = sendCommand_ Command{command = "WebDriver:IsElementEnabled", parameters = Aeson.object []}
--
-- isElementSelected :: (Marionette :> es) => Eff es ()
-- isElementSelected = sendCommand_ Command{command = "WebDriver:IsElementSelected", parameters = Aeson.object []}
--
-- minimizeWindow :: (Marionette :> es) => Eff es ()
-- minimizeWindow = sendCommand_ Command{command = "WebDriver:MinimizeWindow", parameters = Aeson.object []}
--
-- maximizeWindow :: (Marionette :> es) => Eff es ()
-- maximizeWindow = sendCommand_ Command{command = "WebDriver:MaximizeWindow", parameters = Aeson.object []}
--
navigate :: (Marionette :> es) => Text -> Eff es ()
navigate = m . Marionette.navigate

--
newSession :: (Marionette :> es) => Eff es ()
newSession = m Marionette.newSession

--
-- newWindow :: (Marionette :> es) => Eff es ()
-- newWindow = sendCommand_ Command{command = "WebDriver:NewWindow", parameters = Aeson.object []}
--
-- performActions :: (Marionette :> es) => Eff es ()
-- performActions = sendCommand_ Command{command = "WebDriver:PerformActions", parameters = Aeson.object []}
--
-- print :: (Marionette :> es) => Eff es ()
-- print = sendCommand_ Command{command = "WebDriver:Print", parameters = Aeson.object []}
--
-- refresh :: (Marionette :> es) => Eff es ()
-- refresh = sendCommand_ Command{command = "WebDriver:Refresh", parameters = Aeson.object []}
--
-- releaseActions :: (Marionette :> es) => Eff es ()
-- releaseActions = sendCommand_ Command{command = "WebDriver:ReleaseActions", parameters = Aeson.object []}
--
-- sendAlertText :: (Marionette :> es) => Eff es ()
-- sendAlertText = sendCommand_ Command{command = "WebDriver:SendAlertText", parameters = Aeson.object []}
--
-- setPermission :: (Marionette :> es) => Eff es ()
-- setPermission = sendCommand_ Command{command = "WebDriver:SetPermission", parameters = Aeson.object []}
--
-- setTimeouts :: (Marionette :> es) => Eff es ()
-- setTimeouts = sendCommand_ Command{command = "WebDriver:SetTimeouts", parameters = Aeson.object []}
--
-- setWindowRect :: (Marionette :> es) => Eff es ()
-- setWindowRect = sendCommand_ Command{command = "WebDriver:SetWindowRect", parameters = Aeson.object []}
--
-- switchToFrame :: (Marionette :> es) => Eff es ()
-- switchToFrame = sendCommand_ Command{command = "WebDriver:SwitchToFrame", parameters = Aeson.object []}
--
-- switchToParentFrame :: (Marionette :> es) => Eff es ()
-- switchToParentFrame = sendCommand_ Command{command = "WebDriver:SwitchToParentFrame", parameters = Aeson.object []}
--
-- switchToWindow :: (Marionette :> es) => Eff es ()
-- switchToWindow = sendCommand_ Command{command = "WebDriver:SwitchToWindow", parameters = Aeson.object []}
--
-- takeScreenshot :: (Marionette :> es) => Eff es ByteString
-- takeScreenshot =
--     either (throwM . AssertionFailed) (pure . Base64.decodeLenient)
--         . Aeson.parseEither (Aeson.withText "value" $ pure . Text.encodeUtf8)
--         =<< sendCommand
--             Command
--                 { command = "WebDriver:TakeScreenshot"
--                 , parameters = Aeson.object []
--                 }
