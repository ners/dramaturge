{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-poly-kind-signatures #-}
{-# OPTIONS_GHC -Wno-missing-role-annotations #-}

module Effectful.Validate
    ( Validate
    , runValidate
    , refute
    , dispute
    , tolerate
    )
where

import Control.Monad.Except (ExceptT)
import Control.Monad.State.Strict (StateT)
import Control.Monad.State.Strict qualified as State
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Validate (ValidateT)
import Control.Monad.Validate qualified as Validate
import Control.Monad.Validate.Internal (MonoMaybe (..))
import Control.Monad.Validate.Internal qualified as Validate
import Data.Kind (Type)
import Effectful (Dispatch (Static), DispatchOf, Eff, Effect, inject, (:>))
import Effectful.Dispatch.Static
    ( HasCallStack
    , SideEffects (NoSideEffects)
    , StaticRep
    , evalStaticRep
    , getStaticRep
    , putStaticRep
    , runStaticRep
    , unsafeEff_
    )
import Effectful.Exception (throwIO, tryJust)
import ErrorId (ErrorId, ErrorWrapper (ErrorWrapper), matchError, newErrorId)
import GHC.Stack (CallStack, callStack, emptyCallStack, withFrozenCallStack)
import Unsafe.Coerce (unsafeCoerce)
import Prelude

data Validate (e :: Type) :: Effect

type instance DispatchOf (Validate _) = 'Static 'NoSideEffects

data instance StaticRep (Validate e) = Validate ErrorId (Maybe e)

runValidate :: Eff (Validate e ': es) a -> Eff es (Either e a)
runValidate action = do
    eid <- unsafeEff_ newErrorId
    (a, Validate _ e) <- runStaticRep (Validate eid Nothing) $ tryJust (matchError eid) action
    case a of
        Left e -> pure $ Left e
        Right a ->
            case e of
                Just e -> pure $ Left e
                Nothing -> pure $ Right a

foo :: forall e es a. ValidateT e (Eff es) a -> Eff (Validate e ': es) a
foo action = do
    Validate eid mm <- getStaticRep
    case mm of
        Nothing ->
            inject (Validate.unValidateT MNothing action) >>= \case
                Right (MNothing, a) -> putStaticRep @(Validate e) (Validate eid Nothing) >> pure a
                Right (MJust e, a) -> putStaticRep @(Validate e) (Validate eid (Just e)) >> pure a
                Left e -> throwIO $ ErrorWrapper eid "" (unsafeCoerce e)
        Just e ->
            inject (Validate.unValidateT (MJust e) action) >>= \case
                Right (MNothing, _) -> Validate.invalidRestoreError
                Right (MJust e, a) -> putStaticRep (Validate eid (Just e)) >> pure a
                Left e -> throwIO $ ErrorWrapper eid "" (unsafeCoerce e)

unfoo :: forall e es a. ErrorId -> Eff (Validate e ': es) a -> ValidateT e (Eff es) a
unfoo eid action = Validate.ValidateT do
    State.get >>= \case
        MNothing -> do
            (a, m') <- go Nothing
            case m' of
                Nothing -> State.put MNothing
                Just e -> State.put (MJust e)
            pure a
        MJust e -> do
            (a, m') <- go (Just e)
            case m' of
                Nothing -> Validate.invalidRestoreError
                Just e' -> State.put (MJust e')
            pure a
  where
    go :: Maybe e -> StateT (MonoMaybe s e) (ExceptT e (Eff es)) (a, Maybe e)
    go m = lift . lift . evalStaticRep (Validate eid m) $ do
        a <- action
        Validate _ m' <- getStaticRep
        pure (a, m')

refute :: (Semigroup e, Validate e :> es) => e -> Eff es a
refute = inject . foo . Validate.refute

dispute :: (Semigroup e, Validate e :> es) => e -> Eff es ()
dispute = inject . foo . Validate.dispute

tolerate :: forall e es a. (HasCallStack, Semigroup e, Validate e :> es) => Eff es a -> Eff es (Maybe a)
tolerate action = do
    Validate eid _ <- getStaticRep @(Validate e)
    inject . foo . Validate.tolerate . unfoo @e eid . inject $ action
