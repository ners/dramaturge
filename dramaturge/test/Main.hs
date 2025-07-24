module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (newTVarIO)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ReaderT (runReaderT), runReader)
import Data.List.NonEmpty (NonEmpty)
-- import Test.Marionette (Element, Selector (..), navigate, newSession, runMarionette)
-- import Test.Marionette qualified as Marionette

import Effectful
import Effectful.Process (runProcess)
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

main :: IO ()
main = runEff . runProcess . withFirefox True . runMarionette $ do
    newSession
    navigate "https://example.com"

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
