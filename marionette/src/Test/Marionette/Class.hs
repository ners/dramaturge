{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Marionette.Class where

import Control.Monad.Catch (MonadThrow, MonadMask, MonadCatch)
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT)
import Data.Text (Text)
import Test.Marionette.Protocol (Command, Result)
import UnliftIO.STM (TQueue)
import Prelude
import Control.Monad.Reader qualified as Reader
import UnliftIO (MonadUnliftIO)

data CommandWithCallback m = CommandWithCallback Command (Result -> m ())

type role CommandWithCallback representational

data MarionetteState m = MarionetteState
    { sendQueue :: TQueue (CommandWithCallback m)
    , sessionId :: Maybe Text
    }

type role MarionetteState representational

newtype MarionetteT m a = MarionetteT { runMarionetteT :: ReaderT (MarionetteState (MarionetteT m)) m a }
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadThrow
        , MonadCatch
        , MonadMask
        , MonadIO
        , MonadUnliftIO
        , MonadReader (MarionetteState (MarionetteT m))
        )
type role MarionetteT representational nominal

type Marionette m = (MonadReader (MarionetteState m) m, MonadIO m, MonadThrow m)
