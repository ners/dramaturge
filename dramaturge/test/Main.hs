module Main where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (MonadIO (liftIO))
import System.IO (hPrint, stderr)
import Test.Dramaturge
import Test.Marionette
import Prelude

main :: IO ()
main = withFirefox True \_ -> runMarionette do
    newSession
    navigate "https://example.com"
    scrollIntoView =<< findElement (ByXPath "//h1[contains(text(), 'Example Domain')]")
    liftIO . hPrint stderr =<< findElements (ByXPath "//h1[contains(text(), 'Example Domain')]")
