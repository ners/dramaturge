module Main where

import Control.Monad ((<=<))
import Effectful (runEff)
import Effectful.Concurrent (runConcurrent, threadDelay)
import Effectful.Validate
import System.Exit (exitFailure)
import Prelude

main :: IO ()
main =
    print =<< (runEff . runValidate @[String] . runConcurrent) do
        dispute ["!!! BOOM"]
        refute ["!!! BANG"]
        pure ()
