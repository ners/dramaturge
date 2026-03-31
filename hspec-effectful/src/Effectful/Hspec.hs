{-# LANGUAGE RoleAnnotations #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Effectful.Hspec
    ( Hspec
    , runHspec
    , assertFailure
    , shouldBe
    , shouldSatisfy
    , shouldStartWith
    , shouldEndWith
    , shouldContain
    , shouldMatchList
    , shouldReturn
    , shouldNotBe
    , shouldNotSatisfy
    , shouldNotContain
    , shouldNotReturn
    , shouldThrow
    , anyException
    )
where

import Control.Monad (unless)
import Data.Typeable (typeOf)
import Effectful
import Effectful.Dispatch.Static
import Effectful.Exception (Exception (displayException), try)
import GHC.IsList (IsList (..))
import Test.HUnit qualified as HUnit
import Test.Hspec.Expectations (anyException)
import Test.Hspec.Expectations qualified as Hspec
import Prelude

data Hspec :: Effect

type role Hspec phantom phantom

type instance DispatchOf Hspec = 'Static 'WithSideEffects

data instance StaticRep Hspec = Hspec

runHspec :: (IOE :> es) => Eff (Hspec ': es) a -> Eff es a
runHspec = evalStaticRep Hspec

assert :: (Hspec :> es) => Hspec.Expectation -> Eff es ()
assert = unsafeEff_

assertFailure :: (HasCallStack, Hspec :> es) => String -> Eff es a
assertFailure = unsafeEff_ . HUnit.assertFailure

shouldBe :: (HasCallStack, Show a, Eq a, Hspec :> es) => a -> a -> Eff es ()
shouldBe = (assert .) . Hspec.shouldBe

shouldSatisfy
    :: (HasCallStack, Show a, Hspec :> es)
    => a
    -> (a -> Bool)
    -> Eff es ()
shouldSatisfy = (assert .) . Hspec.shouldSatisfy

shouldStartWith
    :: (HasCallStack, Hspec :> es, IsList l, Show (Item l), Eq (Item l))
    => l
    -> l
    -> Eff es ()
xs `shouldStartWith` prefix = assert $ toList xs `Hspec.shouldStartWith` toList prefix

shouldEndWith
    :: (HasCallStack, Hspec :> es, IsList l, Show (Item l), Eq (Item l))
    => l -> l -> Eff es ()
xs `shouldEndWith` prefix = assert $ toList xs `Hspec.shouldEndWith` toList prefix

shouldContain
    :: (HasCallStack, Hspec :> es, IsList l, Show (Item l), Eq (Item l))
    => l
    -> l
    -> Eff es ()
xs `shouldContain` prefix = assert $ toList xs `Hspec.shouldContain` toList prefix

shouldMatchList
    :: (HasCallStack, Hspec :> es, IsList l, Show (Item l), Eq (Item l))
    => l
    -> l
    -> Eff es ()
xs `shouldMatchList` prefix = assert $ toList xs `Hspec.shouldMatchList` toList prefix

shouldReturn
    :: (HasCallStack, Show a, Eq a, Hspec :> es)
    => Eff es a
    -> a
    -> Eff es ()
shouldReturn action expected = action >>= (`shouldBe` expected)

shouldNotBe :: (HasCallStack, Show a, Eq a, Hspec :> es) => a -> a -> Eff es ()
shouldNotBe = (assert .) . Hspec.shouldNotBe

shouldNotSatisfy
    :: (HasCallStack, Show a, Hspec :> es)
    => a
    -> (a -> Bool)
    -> Eff es ()
shouldNotSatisfy = (assert .) . Hspec.shouldNotSatisfy

shouldNotContain
    :: (HasCallStack, Hspec :> es, IsList l, Show (Item l), Eq (Item l))
    => l
    -> l
    -> Eff es ()
xs `shouldNotContain` prefix = assert $ toList xs `Hspec.shouldNotContain` toList prefix

shouldNotReturn
    :: (HasCallStack, Show a, Eq a, Hspec :> es)
    => Eff es a
    -> a
    -> Eff es ()
shouldNotReturn action notExpected = action >>= (`shouldNotBe` notExpected)

shouldThrow
    :: (HasCallStack, Exception e, Hspec :> es)
    => Eff es a -> Hspec.Selector e -> Eff es ()
action `shouldThrow` p =
    try action >>= \case
        Right _ ->
            assertFailure $
                "did not get expected exception: " ++ exceptionType
        Left e ->
            unless (p e) . assertFailure . unlines $
                [ "predicate failed on expected exception: " ++ exceptionType
                , displayException e
                ]
  where
    -- a string representation of the expected exception's type
    exceptionType = (show . typeOf . instanceOf) p
      where
        instanceOf :: Hspec.Selector a -> a
        instanceOf _ = error "Effectful.HUnit.shouldThrow: broken Typeable instance"
