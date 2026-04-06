module Main where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM (newTVarIO)
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Default (def)
import Effectful
import Effectful.Exception (bracket)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant (NamedRoutes, Proxy (Proxy), hoistServer)
import Servant.Server.Generic (genericServe)
import Server (Routes, State, server)
import Test.Dramaturge
import Prelude

startServer :: IO ()
startServer = do
    state :: State <- newTVarIO mempty
    run 8080
        . logStdoutDev
        . genericServe
        . hoistServer (Proxy @(NamedRoutes Routes)) (`runReaderT` state)
        $ server

withServer :: (IOE :> es) => Eff es a -> Eff es a
withServer = bracket (liftIO $ forkIO startServer) (liftIO . killThread) . const

main :: IO ()
main = runEff . withServer . runDramaturge def $ do
    newSession
    navigate "http://localhost:8080"

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
