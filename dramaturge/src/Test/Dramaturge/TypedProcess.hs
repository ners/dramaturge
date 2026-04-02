module Test.Dramaturge.TypedProcess
    ( module Test.Dramaturge.TypedProcess
    , module Effectful.Process.Typed
    )
where

import Data.ByteString.Lazy (LazyByteString)
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Effectful
import Effectful.Process.Typed hiding (readProcessStdout_)
import Effectful.Process.Typed qualified as Process
import GHC.Stack (HasCallStack)
import Test.Dramaturge.Log
import Prelude

readProcessStdout_
    :: (HasCallStack, TypedProcess :> es, Log :> es)
    => ProcessConfig stdin stdout stderr -> Eff es LazyByteString
readProcessStdout_ process = do
    out <- Process.readProcessStdout_ process
    logTrace (Text.pack . show $ process)
        . Text.decodeUtf8
        . LazyByteString.toStrict $
        out
    pure out
