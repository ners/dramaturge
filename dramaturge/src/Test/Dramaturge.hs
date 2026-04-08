{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Dramaturge
    ( module Test.Dramaturge
    , module Test.Dramaturge.Config
    , module Test.Dramaturge.Log
    , module Test.Dramaturge.TypedProcess
    , module Effectful.Concurrent
    , module Effectful.Concurrent.Async
    , module Effectful.Environment
    , module Effectful.Exception
    , module Effectful.Fail
    , module Effectful.FileSystem
    , module Effectful.FileSystem.IO.ByteString
    , module Effectful.Hspec
    , module Effectful.Marionette
    , module Effectful.Retry
    , module Effectful.Timeout
    , module Data.These
    , module GHC.Stack
    )
where

import Control.Monad (unless, (<=<))
import Control.Monad.Catch (MonadThrow (throwM))
import Control.Monad.Extra (untilJustM)
import Data.Aeson (ToJSON (toJSON))
import Data.String (fromString)
import Data.Text (Text)
import Data.These
import Effectful
import Effectful.Concurrent (Concurrent, runConcurrent, threadDelay)
import Effectful.Concurrent.Async (concurrently)
import Effectful.Environment hiding (setEnv)
import Effectful.Exception (Exception (..), SomeException, catch, try)
import Effectful.Fail (Fail, runFailIO)
import Effectful.FileSystem (FileSystem, runFileSystem)
import Effectful.FileSystem.IO.ByteString (writeFile)
import Effectful.Hspec
import Effectful.Marionette
import Effectful.Retry
import Effectful.Timeout (Timeout, runTimeout, timeout)
import GHC.Stack (HasCallStack, withFrozenCallStack)
import Test.Dramaturge.Config
import Test.Dramaturge.Firefox (withFirefox)
import Test.Dramaturge.Log
import Test.Dramaturge.TypedProcess
import Prelude hiding (writeFile)

runDramaturge
    :: (IOE :> es)
    => Config
    -> Eff
        ( Concurrent
            ': Environment
            ': Fail
            ': FileSystem
            ': Hspec
            ': Log
            ': Marionette
            ': Retry
            ': Timeout
            ': TypedProcess
            ': es
        )
        a
    -> Eff es a
runDramaturge Config{..} =
    runConcurrent
        . runEnvironment
        . runFailIO
        . runFileSystem
        . runHspec
        . runLog logLevel
        . runMarionette
        . runRetry
        . runTimeout
        . runTypedProcess
        . withFirefox firefox
        . inject

newtype TimeoutException = TimeoutException String
    deriving anyclass (Exception)

instance Show TimeoutException where
    show (TimeoutException s) = "Timed out waiting for " <> s

withTimeout :: (Timeout :> es) => String -> Int -> Eff es a -> Eff es a
withTimeout (TimeoutException -> e) µs = maybe (throwM e) pure <=< timeout µs

waitFor
    :: forall a es
     . ( HasCallStack
       , Concurrent :> es
       , Log :> es
       , Timeout :> es
       )
    => String
    -> Eff es a
    -> Eff es a
waitFor what action =
    withTimeout what maxDuration . untilJustM $
        (Just <$> action) `catch` \e -> do
            logTrace_ . fromString . displayException @SomeException $ e
            threadDelay stepDuration
            pure Nothing
  where
    maxDuration, stepDuration :: Int
    maxDuration = 5_000_000
    stepDuration = 10_000

waitForElementThat
    :: ( HasCallStack
       , Concurrent :> es
       , Log :> es
       , Marionette :> es
       , Timeout :> es
       )
    => String
    -> (Element -> Eff es ())
    -> Selector
    -> Eff es Element
waitForElementThat what test selector = withFrozenCallStack $ waitFor what do
    e <- findElement selector
    test e
    pure e

waitForElement
    :: ( HasCallStack
       , Concurrent :> es
       , Log :> es
       , Marionette :> es
       , Timeout :> es
       )
    => Selector
    -> Eff es Element
waitForElement selector = waitForElementThat what test selector
  where
    what = "element (" <> show selector <> ")"
    test :: forall a m. (Applicative m) => a -> m ()
    test = const $ pure ()

instance (Exception e) => Exception [e]

tryThese
    :: (HasCallStack, Concurrent :> es)
    => Eff es a
    -> Eff es b
    -> Eff es (These a b)
tryThese a b =
    concurrently (try @SomeException a) (try b) >>= \case
        (Right a, Left _) -> pure (This a)
        (Left _, Right b) -> pure (That b)
        (Right a, Right b) -> pure (These a b)
        (Left a, Left b) -> throwM [a, b]

findTheseElements
    :: ( HasCallStack
       , Concurrent :> es
       , Log :> es
       , Marionette :> es
       )
    => Selector
    -> Selector
    -> Eff es (These Element Element)
findTheseElements s1 s2 = do
    logTraceShow_ (s1, s2)
    withFrozenCallStack $ tryThese (findElement s1) (findElement s2)

scrollIntoView
    :: (HasCallStack, Marionette :> es, Log :> es)
    => Element
    -> Eff es ()
scrollIntoView element = do
    logTraceShow_ element
    executeScript
        "arguments[0].scrollIntoView({behavior: 'instant', block: 'nearest'})"
        [toJSON element]

click :: (HasCallStack, Marionette :> es, Log :> es) => Element -> Eff es ()
click element = do
    scrollIntoView element
    logTraceShow_ element
    elementClick element

getValue
    :: (HasCallStack, Marionette :> es, Log :> es)
    => Element
    -> Eff es Text
getValue element = do
    value <- executeScript "return arguments[0].value" [toJSON element]
    logTraceShow_ value
    pure value

setValue
    :: ( HasCallStack
       , Concurrent :> es
       , Hspec :> es
       , Log :> es
       , Marionette :> es
       , Timeout :> es
       )
    => Text
    -> Element
    -> Eff es ()
setValue value element =
    getValue element >>= flip (unless . (== value)) do
        logTraceShow_ (element, value)
        elementClear element
        waitFor "element to be clear" $ element `shouldHaveValue` ""
        elementSendKeys element value
        waitFor "element to have new value" $ element `shouldHaveValue` value

shouldHaveValue
    :: (HasCallStack, Marionette :> es, Hspec :> es, Log :> es)
    => Element
    -> Text
    -> Eff es ()
shouldHaveValue element value = getValue element `shouldReturn` value

getChecked
    :: (HasCallStack, Marionette :> es, Log :> es)
    => Element
    -> Eff es Bool
getChecked element = do
    checked <- executeScript "return arguments[0].checked" [toJSON element]
    logTraceShow_ checked
    pure checked

setChecked
    :: ( HasCallStack
       , Concurrent :> es
       , Hspec :> es
       , Log :> es
       , Marionette :> es
       , Timeout :> es
       )
    => Bool
    -> Element
    -> Eff es ()
setChecked checked element =
    getChecked element >>= flip (unless . (== checked)) do
        logTraceShow_ (element, checked)
        waitFor "element to have new checked" $ element `shouldHaveChecked` checked

shouldHaveChecked
    :: (HasCallStack, Marionette :> es, Hspec :> es, Log :> es)
    => Element
    -> Bool
    -> Eff es ()
shouldHaveChecked element checked = getChecked element `shouldReturn` checked
