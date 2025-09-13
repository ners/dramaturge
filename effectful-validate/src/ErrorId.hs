module ErrorId where

import Control.Exception (Exception)
import Data.Primitive.ByteArray (MutableByteArray, newByteArray, sameMutableByteArray)
import GHC.Base (Any, RealWorld)
import GHC.Stack (CallStack, prettyCallStack)
import Unsafe.Coerce (unsafeCoerce)
import Prelude

newtype ErrorId = ErrorId Unique
    deriving newtype (Eq)

{- | A unique is picked so that distinct 'Error' handlers for the same type
don't catch each other's exceptions.
-}
newErrorId :: IO ErrorId
newErrorId = ErrorId <$> newUnique

data ErrorWrapper = ErrorWrapper !ErrorId String Any

instance Show ErrorWrapper where
    showsPrec _ (ErrorWrapper _ errRep _) =
        (unwords ["Effectful.Error.Static.ErrorWrapper:", errRep] ++)

instance Exception ErrorWrapper

matchError :: ErrorId -> ErrorWrapper -> Maybe e
matchError eid (ErrorWrapper etag _ e)
    | eid == etag = Just (unsafeCoerce e)
    | otherwise = Nothing

{- | A unique with no possibility for CAS contention.

Credits for this go to Edward Kmett.
-}
newtype Unique = Unique (MutableByteArray RealWorld)

instance Eq Unique where
    Unique a == Unique b = sameMutableByteArray a b

newUnique :: IO Unique
newUnique = Unique <$> newByteArray 0
