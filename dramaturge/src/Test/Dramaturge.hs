{-# LANGUAGE RoleAnnotations #-}

module Test.Dramaturge
    ( module Test.Dramaturge
    , module Effectful.Marionette
    )
where

import Control.Exception (AssertionFailed (AssertionFailed))
import Control.Monad ((>=>))
import Control.Monad.Catch (MonadThrow (throwM))
import Data.Aeson (ToJSON (toJSON))
import Data.Functor.Identity (Identity (..))
import Data.Maybe (isJust)
import Effectful (Dispatch (Static), DispatchOf, Eff, Effect, IOE, (:>))
import Effectful.Dispatch.Static (SideEffects (WithSideEffects), StaticRep, evalStaticRep)
import Effectful.Log (LogLevel)
import GHC.IsList (IsList (Item))
import Effectful.Marionette
import Prelude

data DramaturgeConfig = DramaturgeConfig
    { headless :: Bool
    , logLevel :: LogLevel
    }

data Dramaturge :: Effect

type role Dramaturge phantom phantom

type instance DispatchOf Dramaturge = 'Static 'WithSideEffects

newtype instance StaticRep Dramaturge = Dramaturge DramaturgeConfig

-- runDramaturge :: (IOE :> es) => DramaturgeConfig -> Eff (Dramaturge ': es) a -> Eff es a
-- runDramaturge conf effect = do
--     -- session <- liftIO $ Dramaturge.runSession conf Dramaturge.getSession
--     evalStaticRep (Dramaturge conf) effect
--
-- scrollIntoView :: (Dramaturge :> es) => Element -> Eff es ()
-- scrollIntoView = executeScript "arguments[0].scrollIntoView({behavior: 'instant', block: 'nearest'})" . Identity . toJSON
--
-- isVisible :: (Dramaturge :> es) => Element -> Eff es Bool
-- isVisible = executeScript "arguments[0].checkVisibility()" . Identity . toJSON
--
-- isDisabled :: (Dramaturge :> es) => Element -> Eff es Bool
-- isDisabled = fmap isJust . Marionette.getElementAttribute "disabled"
--
-- isEnabled :: (Dramaturge :> es) => Element -> Eff es Bool
-- isEnabled = fmap not . isDisabled
--
-- findAll :: (Dramaturge :> es, IsList list, Item list ~ Element) => Selector -> Eff es list
-- findAll = Marionette.findElements
--
-- findOne :: (Dramaturge :> es) => Selector -> Eff es Element
-- findOne =
--     findAll >=> \case
--         [e] -> pure e
--         _ -> throwM . AssertionFailed $ "findOne: expected to find one element"
--
-- click :: (Dramaturge :> es) => Element -> Eff es ()
-- click e = do
--     Marionette.elementClick e
