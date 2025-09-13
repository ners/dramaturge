{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (newTVarIO)
-- import Test.Marionette (Element, Selector (..), navigate, newSession, runMarionette)
-- import Test.Marionette qualified as Marionette

import Control.Exception (Exception (..), throwIO)
import Control.Monad.Extra (eitherM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ReaderT (runReaderT), runReader)
import Control.Monad.Validate (MonadValidate (tolerate))
import Data.List.NonEmpty (NonEmpty)
import Effectful
import Effectful.Error.Static (CallStack, prettyCallStack)
import Effectful.Process (runProcess)
import GHC.Exception (prettyCallStackLines)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant (NamedRoutes, Proxy (Proxy), hoistServer, serve)
import Servant.Server.Generic (genericServe)
import Server (Routes, State, server)
import System.IO (hPrint, stderr)
import Test.Dramaturge
import Test.Dramaturge.Firefox (withFirefox)
import Test.Dramaturge.Marionette
import Prelude

-- main = do
--     state :: State <- newTVarIO mempty
--     run 8080
--         . logStdoutDev
--         . genericServe
--         . hoistServer (Proxy @(NamedRoutes Routes)) (`runReaderT` state)
--         $ server

instance {-# OVERLAPPING #-} (Exception e) => Show (NonEmpty (CallStack, e)) where
    show = unlines . ("" :) . concatMap (\(cs, e) -> displayException e : drop 1 (prettyCallStackLines cs))

instance (Exception e) => Exception (NonEmpty (CallStack, e))

main :: IO ()
main = eitherM throwIO pure . runEff . runProcess . withFirefox True . runMarionette $ do
    newSession
    navigate "https://example.com"
    elementClick =<< findElement (ByXPath "//h1[contains(text(), 'Example Domain 1')]")
    elementClick =<< findElement (ByXPath "//h1[contains(text(), 'Example Domain 2')]")
    elementClick =<< findElement (ByXPath "//h1[contains(text(), 'Example Domain 3')]")
    pure ()

-- scrollIntoView =<< findOne (ByXPath "//h1[contains(text(), 'Example Domain')]")
-- e <- findOne (ByXPath "//h1[contains(text(), 'Example Domain')]")
-- liftIO . print =<< Marionette.getElementRect e
-- liftIO . print =<< isVisible e
-- liftIO . print =<< Marionette.getElementAttribute "disabled" e
-- liftIO . hPrint @(NonEmpty Element) stderr =<< findAll (ByXPath "//h1[contains(text(), 'Example Domain')]")
--
-- handlerServer :: ServerT MyServerType Handler  -- This code is the important part where we convert a value of type `ServerT MyServerType (Reader String)` to a value of type `ServerT MyServerType Handler`, using the hoistServer function from Servant.
-- handlerServer = hoistServer api readerToHandler readerServer
--  where
--   readerToHandler :: Reader String x -> Handler x  -- This code just extracts the value from our custom monads (Reader here) and wraps it in the Handler monad.
--   readerToHandler r = return $ runReader r "reader env"
