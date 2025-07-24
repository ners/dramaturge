{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Marionette.Protocol where

import Control.Monad.Catch (Exception (..), MonadThrow (throwM))
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), Value (..), withArray)
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Bifunctor (Bifunctor (first))
import Data.Binary (Binary (..), getWord8)
import Data.Binary qualified as Binary
import Data.Binary.Parser (decimal, getLazyByteString)
import Data.Binary.Put (putLazyByteString)
import Data.ByteString.Lazy (LazyByteString)
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Char (chr)
import Data.Either (fromRight)
import Data.Foldable qualified as Foldable
import Data.String (IsString (fromString))
import Data.Text
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import GHC.Generics (Generic)
import GHC.IsList (IsList (fromList))
import Prelude hiding (log)

data Message a = Message {messageId :: Int, messageContent :: a}
    deriving stock (Show)

type role Message representational

newtype MarionetteMessage = MarionetteMessage LazyByteString

instance Binary MarionetteMessage where
    put :: MarionetteMessage -> Binary.Put
    put (MarionetteMessage lbs) = putLazyByteString $ (fromString . show . LazyByteString.length $ lbs) <> ":" <> lbs
    get :: Binary.Get MarionetteMessage
    get = do
        len <- decimal
        ':' <- chr . fromIntegral <$> getWord8
        MarionetteMessage <$> getLazyByteString len

instance Show MarionetteMessage where
    show :: MarionetteMessage -> String
    show = Text.unpack . Text.decodeUtf8 . LazyByteString.toStrict . Binary.encode

decodeMarionette :: (FromJSON a) => MarionetteMessage -> Either String a
decodeMarionette (MarionetteMessage lbs) = Aeson.eitherDecode lbs

data Command = Command
    { command :: Text
    , parameters :: Value
    }
    deriving stock (Show)

instance ToJSON (Message Command) where
    toJSON :: Message Command -> Value
    toJSON Message{messageId, messageContent = Command{..}} = Array . fromList $ [Number 0, toJSON messageId, toJSON command, parameters]

instance FromJSON (Message Command) where
    parseJSON :: Value -> Aeson.Parser (Message Command)
    parseJSON = withArray "command" \a -> do
        [Number 0, messageIdVal, String command, parameters] <- pure $ Foldable.toList a
        messageId <- parseJSON messageIdVal
        pure $ Message messageId $ Command{..}

type Result = Either Error Value

instance ToJSON (Message Result) where
    toJSON :: Message Result -> Value
    toJSON Message{..} = Array . fromList $ [Number 1, toJSON messageId, errVal, resVal]
      where
        errVal = either toJSON (const Null) messageContent
        resVal = fromRight Null messageContent

instance FromJSON (Message Result) where
    parseJSON :: Value -> Aeson.Parser (Message Result)
    parseJSON = withArray "result" \a -> do
        [Number 1, messageIdVal, err, res] <- pure $ Foldable.toList a
        messageId <- parseJSON messageIdVal
        Message messageId
            <$> if err == Null
                then pure . Right $ res
                else Left <$> parseJSON err

data Error = Error
    { error :: Text
    , message :: Text
    , stacktrace :: Text
    }
    deriving stock (Generic)
    deriving anyclass (Exception, FromJSON, ToJSON)

instance Show Error where
    show = Text.unpack . message

data Greeting = Greeting
    { applicationType :: Text
    , marionetteProtocol :: Int
    }
    deriving stock (Generic, Show)
    deriving anyclass (FromJSON)
