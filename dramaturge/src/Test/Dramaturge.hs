{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Dramaturge
    ( module Test.Dramaturge
    , module Test.Dramaturge.Log
    , module Test.Dramaturge.Config
    , module Effectful.Concurrent
    , module Effectful.Concurrent.Async
    , module Effectful.Exception
    , module Effectful.FileSystem
    , module Effectful.FileSystem.IO.ByteString
    , module Effectful.Hspec
    , module Effectful.Marionette
    , module Effectful.Process.Typed
    , module Effectful.Timeout
    , module GHC.Stack
    )
where

import Control.Monad ((<=<))
import Control.Monad.Catch (MonadThrow (throwM))
import Control.Monad.Extra (untilJustM)
import Data.Aeson (ToJSON (toJSON))
import Data.String (fromString)
import Data.These (These (..))
import Effectful
import Effectful.Concurrent (Concurrent, runConcurrent, threadDelay)
import Effectful.Concurrent.Async (concurrently)
import Effectful.Exception (Exception (..), SomeException, catch, try)
import Effectful.FileSystem (FileSystem, runFileSystem)
import Effectful.FileSystem.IO.ByteString (writeFile)
import Effectful.Hspec
import Effectful.Marionette
import Effectful.Process.Typed
import Effectful.Timeout (Timeout, runTimeout, timeout)
import GHC.Stack (HasCallStack, withFrozenCallStack)
import Test.Dramaturge.Config
import Test.Dramaturge.Firefox (withFirefox)
import Test.Dramaturge.Log
import Prelude hiding (writeFile)

runDramaturge
    :: (IOE :> es)
    => Config
    -> Eff
        ( Concurrent
            ': FileSystem
            ': Hspec
            ': Timeout
            ': TypedProcess
            ': Log
            ': Marionette
            ': es
        )
        a
    -> Eff es a
runDramaturge Config{..} =
    runConcurrent
        . runFileSystem
        . runHspec
        . runTimeout
        . runTypedProcess
        . withFirefox firefox
        . runMarionette
        . runLog logLevel
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
       , Timeout :> es
       , Log :> es
       )
    => String
    -> Eff es a
    -> Eff es a
waitFor what action =
    withTimeout what maxDuration . untilJustM $ do
        (Just <$> action) `catch` \(e :: SomeException) -> do
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
       , Timeout :> es
       , Log :> es
       , Marionette :> es
       )
    => (Element -> Eff es ())
    -> Selector
    -> Eff es Element
waitForElementThat test selector = withFrozenCallStack $ waitFor what do
    e <- findElement selector
    test e
    pure e
  where
    what = "element (" <> show selector <> ")"

waitForElement
    :: ( HasCallStack
       , Concurrent :> es
       , Timeout :> es
       , Log :> es
       , Marionette :> es
       )
    => Selector
    -> Eff es Element
waitForElement = waitForElementThat (const $ pure ())

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

findTheseElems
    :: ( HasCallStack
       , Concurrent :> es
       , Log :> es
       , Marionette :> es
       )
    => Selector
    -> Selector
    -> Eff es (These Element Element)
findTheseElems s1 s2 = do
    logTraceShow_ (s1, s2)
    withFrozenCallStack $ tryThese (findElement s1) (findElement s2)

scrollIntoView
    :: (HasCallStack, Marionette :> es, Log :> es)
    => Element
    -> Eff es ()
scrollIntoView e = do
    logTraceShow_ e
    executeScript
        "arguments[0].scrollIntoView({behavior: 'instant', block: 'nearest'})"
        [toJSON e]

click :: (HasCallStack, Marionette :> es, Log :> es) => Element -> Eff es ()
click e = do
    scrollIntoView e
    logTraceShow_ e
    elementClick e
