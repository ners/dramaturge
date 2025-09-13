{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Dramaturge.Marionette
    ( module Test.Dramaturge.Marionette
    , module Test.Marionette.Commands
    )
where

import Control.Monad ((<=<))
import Control.Monad.Except (ExceptT, MonadError, mapError, runExceptT)
import Control.Monad.Except qualified as MonadError
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Validate (ValidateT, refute, runValidateT)
import Data.Aeson (FromJSON)
import Data.Bifunctor (Bifunctor (first))
import Data.Foldable1 (Foldable1 (fold1))
import Data.Foldable1 qualified as Foldable1
import Data.List.NonEmpty (NonEmpty)
import Data.Sequence.NonEmpty (NESeq)
import Data.Text (Text)
import Data.Void (absurd)
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
    , runEff
    , withEffToIO
    , (:>)
    )
import Effectful.Concurrent.Async (mapConcurrently, runConcurrent)
import Effectful.Concurrent.STM (Concurrent, TQueue, atomically, newEmptyTMVarIO, readTMVar, writeTQueue)
import Effectful.Dispatch.Static
    ( SideEffects (WithSideEffects)
    , StaticRep
    , evalStaticRep
    , getStaticRep
    , unsafeEff_
    )
import Effectful.Error.Static (CallStack, Error, HasCallStack, catchError, runError, throwError)
import Effectful.Error.Static qualified as Error
import Effectful.Reader.Static (Reader, runReader)
import Effectful.Reader.Static qualified as Reader
import GHC.IsList (IsList (Item))
import GHC.Stack (callStack, withFrozenCallStack)
import Test.Marionette.Client
    ( CommandWithCallback (CommandWithCallback)
    , MarionetteClient (..)
    )
import Test.Marionette.Client qualified as Marionette
import Test.Marionette.Commands (Element (..), Rect (..), Selector (..))
import Test.Marionette.Commands qualified as Marionette
import Test.Marionette.Protocol qualified as Marionette
import Prelude

type Validate = ValidateT (NESeq (CallStack, Marionette.Error))

type ValidateEff es = Validate (Eff es)

instance
    ( Concurrent :> es
    , Reader (TQueue CommandWithCallback) :> es
    , IOE :> es
    )
    => MarionetteClient (ValidateEff es)
    where
    sendCommand :: (HasCallStack, FromJSON a) => Marionette.Command -> ValidateEff es a
    sendCommand command = do
        q <- lift Reader.ask
        result <- lift newEmptyTMVarIO
        lift . atomically . writeTQueue q $ CommandWithCallback command result
        either (refute . pure . (callStack,)) pure =<< Marionette.parseResult =<< (lift . atomically . readTMVar) result
    sendCommands :: (HasCallStack, Traversable t, FromJSON a) => t Marionette.Command -> ValidateEff es (t a)
    sendCommands = traverse (either refute pure) <=< lift . mapConcurrently (runValidateT . sendCommand)

instance (Error Marionette.Error :> es) => MonadError Marionette.Error (Eff es) where
    throwError :: Marionette.Error -> Eff es a
    throwError = Error.throwError
    catchError :: Eff es a -> (Marionette.Error -> Eff es a) -> Eff es a
    catchError v = Error.catchError v . const

-- instance (Error Marionette.Error :> es, Semigroup e') => MonadError Marionette.Error (ValidateT e' (Eff es)) where
--     throwError :: (CallStack, Marionette.Error) -> ValidateT e' (Eff es) a
--     throwError = lift . throwError
--     catchError :: ValidateT e' (Eff es) a -> ((CallStack, Marionette.Error) -> ValidateT e' (Eff es) a) -> ValidateT e' (Eff es) a
--     catchError v f = either refute pure =<< lift (catchError @(CallStack, Marionette.Error) (runValidateT v) (const $ runValidateT . f))
-- instance
--     ( Concurrent :> es
--     , Reader (TQueue CommandWithCallback) :> es
--     , Error Marionette.Error :> es
--     , IOE :> es
--     )
--     => MarionetteClient (ExceptT (CallStack, Marionette.Error) (ValidateEff es))
--     where
--     sendCommand = lift . sendCommand
--     sendCommands = lift . sendCommands

data Marionette :: Effect

type role Marionette phantom phantom

type instance DispatchOf Marionette = 'Static 'WithSideEffects

newtype instance StaticRep Marionette = Marionette (TQueue CommandWithCallback)

runMarionette :: (HasCallStack, IOE :> es) => ValidateEff (Marionette ': Error Marionette.Error ': es) a -> Eff es (Either (NonEmpty (CallStack, Marionette.Error)) a)
runMarionette action =
    withEffToIO (ConcUnlift Ephemeral (Limited 1)) \unlift -> Marionette.runMarionette do
        sendQueue <- Marionette.getSendQueue
        result <- liftIO . unlift . runError . evalStaticRep (Marionette sendQueue) $ runValidateT action
        pure $ either (Left . pure) (first Foldable1.toNonEmpty) result

-- | Report errors to the ValidateT layer.
mv
    :: forall es a
     . ( HasCallStack
       , Marionette :> es
       , Error Marionette.Error :> es
       )
    => ValidateEff
        '[ Concurrent
         , Reader (TQueue CommandWithCallback)
         , Error Marionette.Error
         , IOE
         ]
        a
    -> ValidateEff es a
mv action = do
    Marionette q <- lift getStaticRep
    result <- lift . unsafeEff_ . runEff . runError . runReader q . runConcurrent . runValidateT $ action
    either (lift . throwError . snd) (either refute pure) result

-- | Report errors to the Error effect which bypasses the ValidateT layer. These errors cannot be tolerated.
me
    :: forall es a
     . ( HasCallStack
       , Marionette :> es
       , Error Marionette.Error :> es
       )
    => ValidateEff
        '[ Concurrent
         , Reader (TQueue CommandWithCallback)
         , Error Marionette.Error
         , IOE
         ]
        a
    -> ValidateEff es a
me = either (absurd . fold1 <=< mapM (lift . throwError . snd)) pure <=< lift . runValidateT . mv

quit :: (HasCallStack, Marionette :> es, Error Marionette.Error :> es) => ValidateEff es ()
quit = withFrozenCallStack $ me Marionette.quit

acceptAlert :: (HasCallStack, Marionette :> es, Error Marionette.Error :> es) => ValidateEff es ()
acceptAlert = withFrozenCallStack $ mv Marionette.acceptAlert

-- acceptDialog :: (HasCallStack, Marionette :> es, Error Marionette.Error :> es) => Eff es ()
-- acceptDialog = sendCommand_ Command{command = "WebDriver:AcceptDialog", parameters = Aeson.object []}
--
-- addCookie :: (HasCallStack, Marionette :> es, Error Marionette.Error :> es) => Eff es ()
-- addCookie = sendCommand_ Command{command = "WebDriver:AddCookie", parameters = Aeson.object []}
--
-- back :: (HasCallStack, Marionette :> es, Error Marionette.Error :> es) => Eff es ()
-- back = sendCommand_ Command{command = "WebDriver:Back", parameters = Aeson.object []}
--
-- closeChromeWindow :: (HasCallStack, Marionette :> es, Error Marionette.Error :> es) => Eff es ()
-- closeChromeWindow = sendCommand_ Command{command = "WebDriver:CloseChromeWindow", parameters = Aeson.object []}
--
-- closeWindow :: (HasCallStack, Marionette :> es, Error Marionette.Error :> es) => Eff es ()
-- closeWindow = sendCommand_ Command{command = "WebDriver:CloseWindow", parameters = Aeson.object []}
--
-- deleteAllCookies :: (HasCallStack, Marionette :> es, Error Marionette.Error :> es) => Eff es ()
-- deleteAllCookies = sendCommand_ Command{command = "WebDriver:DeleteAllCookies", parameters = Aeson.object []}
--
-- deleteCookie :: (HasCallStack, Marionette :> es, Error Marionette.Error :> es) => Eff es ()
-- deleteCookie = sendCommand_ Command{command = "WebDriver:DeleteCookie", parameters = Aeson.object []}
--
-- deleteSession :: (HasCallStack, Marionette :> es, Error Marionette.Error :> es) => Eff es ()
-- deleteSession = sendCommand_ Command{command = "WebDriver:DeleteSession", parameters = Aeson.object []}
--
-- dismissAlert :: (HasCallStack, Marionette :> es, Error Marionette.Error :> es) => Eff es ()
-- dismissAlert = sendCommand_ Command{command = "WebDriver:DismissAlert", parameters = Aeson.object []}
--
-- elementClear :: (HasCallStack, Marionette :> es, Error Marionette.Error :> es) => Eff es ()
-- elementClear = sendCommand_ Command{command = "WebDriver:ElementClear", parameters = Aeson.object []}

elementClick :: (HasCallStack, Marionette :> es, Error Marionette.Error :> es) => Element -> ValidateEff es ()
elementClick = withFrozenCallStack $ mv . Marionette.elementClick

-- elementSendKeys :: (HasCallStack, Marionette :> es, Error Marionette.Error :> es) => Element -> Text -> Eff es ()
-- elementSendKeys Element{..} text =
--     sendCommand_
--         Command
--             { command = "WebDriver:ElementSendKeys"
--             , parameters = Aeson.object ["id" .= elementId, "text" .= text]
--             }
--
-- executeAsyncScript :: (HasCallStack, Marionette :> es, Error Marionette.Error :> es) => Eff es ()
-- executeAsyncScript = sendCommand_ Command{command = "WebDriver:ExecuteAsyncScript", parameters = Aeson.object []}
--
-- executeScript :: (HasCallStack, Marionette :> es, Foldable f, FromJSON a) => Text -> f Value -> Eff es a
-- executeScript script args =
--     value
--         <$> sendCommand
--             Command
--                 { command = "WebDriver:ExecuteScript"
--                 , parameters = Aeson.object ["script" .= script, "args" .= Foldable.toList args]
--                 }

findElement :: (HasCallStack, Marionette :> es, Error Marionette.Error :> es) => Selector -> ValidateEff es Element
findElement = withFrozenCallStack $ mv . Marionette.findElement

findElementFrom :: (HasCallStack, Marionette :> es, Error Marionette.Error :> es) => Element -> Selector -> ValidateEff es Element
findElementFrom = withFrozenCallStack $ (mv .) . Marionette.findElementFrom

-- -- findElementFromShadowRoot :: (HasCallStack, Marionette :> es, Error Marionette.Error :> es) => Eff es ()
-- -- findElementFromShadowRoot = sendCommand_ Command{command = "WebDriver:FindElementFromShadowRoot", parameters = Aeson.object []}
--
findElements :: (HasCallStack, Marionette :> es, Error Marionette.Error :> es, IsList list, Item list ~ Element) => Selector -> ValidateEff es list
findElements = withFrozenCallStack $ mv . Marionette.findElements

findElementsFrom :: (HasCallStack, Marionette :> es, Error Marionette.Error :> es, IsList list, Item list ~ Element) => Element -> Selector -> ValidateEff es list
findElementsFrom = withFrozenCallStack $ (mv .) . Marionette.findElementsFrom

--
-- -- findElementsFromShadowRoot :: (HasCallStack, Marionette :> es, Error Marionette.Error :> es) => Eff es ()
-- -- findElementsFromShadowRoot = sendCommand_ Command{command = "WebDriver:FindElementsFromShadowRoot", parameters = Aeson.object []}
--
-- forward :: (HasCallStack, Marionette :> es, Error Marionette.Error :> es) => Eff es ()
-- forward = sendCommand_ Command{command = "WebDriver:Forward", parameters = Aeson.object []}
--
-- fullscreenWindow :: (HasCallStack, Marionette :> es, Error Marionette.Error :> es) => Eff es ()
-- fullscreenWindow = sendCommand_ Command{command = "WebDriver:FullscreenWindow", parameters = Aeson.object []}
--
-- getActiveElement :: (HasCallStack, Marionette :> es, Error Marionette.Error :> es) => Eff es ()
-- getActiveElement = sendCommand_ Command{command = "WebDriver:GetActiveElement", parameters = Aeson.object []}
--
-- getAlertText :: (HasCallStack, Marionette :> es, Error Marionette.Error :> es) => Eff es ()
-- getAlertText = sendCommand_ Command{command = "WebDriver:GetAlertText", parameters = Aeson.object []}
--
-- getCapabilities :: (HasCallStack, Marionette :> es, Error Marionette.Error :> es) => Eff es ()
-- getCapabilities = sendCommand_ Command{command = "WebDriver:GetCapabilities", parameters = Aeson.object []}
--
-- getComputedLabel :: (HasCallStack, Marionette :> es, Error Marionette.Error :> es) => Eff es ()
-- getComputedLabel = sendCommand_ Command{command = "WebDriver:GetComputedLabel", parameters = Aeson.object []}
--
-- getComputedRole :: (HasCallStack, Marionette :> es, Error Marionette.Error :> es) => Eff es ()
-- getComputedRole = sendCommand_ Command{command = "WebDriver:GetComputedRole", parameters = Aeson.object []}
--
-- getCookies :: (HasCallStack, Marionette :> es, Error Marionette.Error :> es) => Eff es ()
-- getCookies = sendCommand_ Command{command = "WebDriver:GetCookies", parameters = Aeson.object []}

getCurrentURL :: (HasCallStack, Marionette :> es, Error Marionette.Error :> es) => ValidateEff es ()
getCurrentURL = withFrozenCallStack $ mv Marionette.getCurrentURL

getElementAttribute :: (HasCallStack, Marionette :> es, Error Marionette.Error :> es) => Text -> Element -> ValidateEff es (Maybe Text)
getElementAttribute = withFrozenCallStack $ (mv .) . Marionette.getElementAttribute

-- getElementCSSValue :: (HasCallStack, Marionette :> es, Error Marionette.Error :> es) => Eff es ()
-- getElementCSSValue = sendCommand_ Command{command = "WebDriver:GetElementCSSValue", parameters = Aeson.object []}
--
-- getElementProperty :: (HasCallStack, Marionette :> es, Error Marionette.Error :> es) => Eff es ()
-- getElementProperty = sendCommand_ Command{command = "WebDriver:GetElementProperty", parameters = Aeson.object []}
--
-- getElementRect :: (HasCallStack, Marionette :> es, Error Marionette.Error :> es) => Element -> Eff es Rect
-- getElementRect Element{..} = sendCommand Command{command = "WebDriver:GetElementRect", parameters = Aeson.object ["id" .= elementId]}
--
-- getElementTagName :: (HasCallStack, Marionette :> es, Error Marionette.Error :> es) => Eff es ()
-- getElementTagName = sendCommand_ Command{command = "WebDriver:GetElementTagName", parameters = Aeson.object []}
--
-- getElementText :: (HasCallStack, Marionette :> es, Error Marionette.Error :> es) => Eff es ()
-- getElementText = sendCommand_ Command{command = "WebDriver:GetElementText", parameters = Aeson.object []}
--
-- getPageSource :: (HasCallStack, Marionette :> es, Error Marionette.Error :> es) => Eff es ()
-- getPageSource = sendCommand_ Command{command = "WebDriver:GetPageSource", parameters = Aeson.object []}
--
-- getShadowRoot :: (HasCallStack, Marionette :> es, Error Marionette.Error :> es) => Eff es ()
-- getShadowRoot = sendCommand_ Command{command = "WebDriver:GetShadowRoot", parameters = Aeson.object []}
--
-- getTimeouts :: (HasCallStack, Marionette :> es, Error Marionette.Error :> es) => Eff es ()
-- getTimeouts = sendCommand_ Command{command = "WebDriver:GetTimeouts", parameters = Aeson.object []}
--
-- getTitle :: (HasCallStack, Marionette :> es, Error Marionette.Error :> es) => Eff es ()
-- getTitle = sendCommand_ Command{command = "WebDriver:GetTitle", parameters = Aeson.object []}
--
-- getWindowHandle :: (HasCallStack, Marionette :> es, Error Marionette.Error :> es) => Eff es ()
-- getWindowHandle = sendCommand_ Command{command = "WebDriver:GetWindowHandle", parameters = Aeson.object []}
--
-- getWindowHandles :: (HasCallStack, Marionette :> es, Error Marionette.Error :> es) => Eff es ()
-- getWindowHandles = sendCommand_ Command{command = "WebDriver:GetWindowHandles", parameters = Aeson.object []}
--
-- getWindowRect :: (HasCallStack, Marionette :> es, Error Marionette.Error :> es) => Eff es ()
-- getWindowRect = sendCommand_ Command{command = "WebDriver:GetWindowRect", parameters = Aeson.object []}
--
-- isElementDisplayed :: (HasCallStack, Marionette :> es, Error Marionette.Error :> es) => Eff es ()
-- isElementDisplayed = sendCommand_ Command{command = "WebDriver:IsElementDisplayed", parameters = Aeson.object []}
--
-- isElementEnabled :: (HasCallStack, Marionette :> es, Error Marionette.Error :> es) => Eff es ()
-- isElementEnabled = sendCommand_ Command{command = "WebDriver:IsElementEnabled", parameters = Aeson.object []}
--
-- isElementSelected :: (HasCallStack, Marionette :> es, Error Marionette.Error :> es) => Eff es ()
-- isElementSelected = sendCommand_ Command{command = "WebDriver:IsElementSelected", parameters = Aeson.object []}
--
-- minimizeWindow :: (HasCallStack, Marionette :> es, Error Marionette.Error :> es) => Eff es ()
-- minimizeWindow = sendCommand_ Command{command = "WebDriver:MinimizeWindow", parameters = Aeson.object []}
--
-- maximizeWindow :: (HasCallStack, Marionette :> es, Error Marionette.Error :> es) => Eff es ()
-- maximizeWindow = sendCommand_ Command{command = "WebDriver:MaximizeWindow", parameters = Aeson.object []}

navigate :: (HasCallStack, Marionette :> es, Error Marionette.Error :> es) => Text -> ValidateEff es ()
navigate = withFrozenCallStack $ mv . Marionette.navigate

newSession :: (HasCallStack, Marionette :> es, Error Marionette.Error :> es) => ValidateEff es ()
newSession = me Marionette.newSession

newWindow :: (HasCallStack, Marionette :> es, Error Marionette.Error :> es) => ValidateEff es ()
newWindow = me Marionette.newWindow

-- performActions :: (HasCallStack, Marionette :> es, Error Marionette.Error :> es) => Eff es ()
-- performActions = sendCommand_ Command{command = "WebDriver:PerformActions", parameters = Aeson.object []}
--
-- print :: (HasCallStack, Marionette :> es, Error Marionette.Error :> es) => Eff es ()
-- print = sendCommand_ Command{command = "WebDriver:Print", parameters = Aeson.object []}
--
-- refresh :: (HasCallStack, Marionette :> es, Error Marionette.Error :> es) => Eff es ()
-- refresh = sendCommand_ Command{command = "WebDriver:Refresh", parameters = Aeson.object []}
--
-- releaseActions :: (HasCallStack, Marionette :> es, Error Marionette.Error :> es) => Eff es ()
-- releaseActions = sendCommand_ Command{command = "WebDriver:ReleaseActions", parameters = Aeson.object []}
--
-- sendAlertText :: (HasCallStack, Marionette :> es, Error Marionette.Error :> es) => Eff es ()
-- sendAlertText = sendCommand_ Command{command = "WebDriver:SendAlertText", parameters = Aeson.object []}
--
-- setPermission :: (HasCallStack, Marionette :> es, Error Marionette.Error :> es) => Eff es ()
-- setPermission = sendCommand_ Command{command = "WebDriver:SetPermission", parameters = Aeson.object []}
--
-- setTimeouts :: (HasCallStack, Marionette :> es, Error Marionette.Error :> es) => Eff es ()
-- setTimeouts = sendCommand_ Command{command = "WebDriver:SetTimeouts", parameters = Aeson.object []}
--
-- setWindowRect :: (HasCallStack, Marionette :> es, Error Marionette.Error :> es) => Eff es ()
-- setWindowRect = sendCommand_ Command{command = "WebDriver:SetWindowRect", parameters = Aeson.object []}
--
-- switchToFrame :: (HasCallStack, Marionette :> es, Error Marionette.Error :> es) => Eff es ()
-- switchToFrame = sendCommand_ Command{command = "WebDriver:SwitchToFrame", parameters = Aeson.object []}
--
-- switchToParentFrame :: (HasCallStack, Marionette :> es, Error Marionette.Error :> es) => Eff es ()
-- switchToParentFrame = sendCommand_ Command{command = "WebDriver:SwitchToParentFrame", parameters = Aeson.object []}
--
-- switchToWindow :: (HasCallStack, Marionette :> es, Error Marionette.Error :> es) => Eff es ()
-- switchToWindow = sendCommand_ Command{command = "WebDriver:SwitchToWindow", parameters = Aeson.object []}
--
-- takeScreenshot :: (HasCallStack, Marionette :> es, Error Marionette.Error :> es) => Eff es ByteString
-- takeScreenshot =
--     either (throwM . AssertionFailed) (pure . Base64.decodeLenient)
--         . Aeson.parseEither (Aeson.withText "value" $ pure . Text.encodeUtf8)
--         =<< sendCommand
--             Command
--                 { command = "WebDriver:TakeScreenshot"
--                 , parameters = Aeson.object []
--                 }
