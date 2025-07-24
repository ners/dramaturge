{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-kind-signatures #-}
{-# OPTIONS_GHC -Wno-missing-poly-kind-signatures #-}
{-# OPTIONS_GHC -Wno-missing-role-annotations #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Server where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Reader qualified as Reader
import Data.Aeson (Value (Null))
import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (LazyByteString)
import Data.ByteString.Lazy qualified as LazyByteString
import Data.IntMap qualified as IntMap
import Data.IntMap.Strict (IntMap)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.Lazy qualified as LazyText
import GHC.Generics (Generic)
import Network.HTTP.Media ((//), (/:))
import Servant
import Servant.Server.Generic
import Text.Blaze.Html (ToMarkup (toMarkup), preEscapedLazyText, preEscapedString, preEscapedText, text)
import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import Text.Hamlet (Html, hamlet, shamlet)
import Text.Julius (RawJS (rawJS), julius, renderJavascript, renderJavascriptUrl)
import UnliftIO.STM (TVar, atomically, readTVar, readTVarIO, stateTVar)
import Prelude

data Routes mode = Routes
    { index :: mode :- Get '[Html] Html
    , api :: mode :- "api" :> NamedRoutes ApiRoutes
    }
    deriving stock (Generic)

data ApiRoutes mode = ApiRoutes
    { getAll :: mode :- Get '[JSON] [(Int, Value)]
    , getOne :: mode :- Capture "id" Int :> Get '[JSON] (Int, Value)
    , post :: mode :- ReqBody '[JSON, Text] Value :> Post '[JSON] (Int, Value)
    , put :: mode :- Capture "id" Int :> ReqBody '[JSON] Value :> Put '[JSON] (Int, Value)
    , deleteOne :: mode :- Capture "id" Int :> Delete '[JSON] (Int, Value)
    , deleteAll :: mode :- Delete '[JSON] [(Int, Value)]
    }
    deriving stock (Generic)

type State = TVar (IntMap Value)

instance ToMarkup Value where
    toMarkup = text . Text.decodeUtf8 . LazyByteString.toStrict . Aeson.encodePretty

server :: (MonadIO m, MonadReader State m, MonadError ServerError m) => Routes (AsServerT m)
server =
    Routes
        { index = do
            items <- withMap_ IntMap.toList
            pure
                [shamlet|
                    <script>
                      const newItem = (body) => fetch(`/api`, { method: 'POST', body });
                      const editItem = (i, body) => fetch(`/api/${i}`, { method: 'POST', body });
                      const deleteItem = (i) => fetch(`/api/${i}`, { method: 'DELETE' });
                    <div>Hello world!
                    <p>Here are your wonderful items:
                    <table>
                        $forall (i, v) <- items
                            <tr>
                                <td>#{i}
                                <td>
                                    <form onsubmit="editItem(#{i}, this.elements['value'].value)">
                                        <textarea name="value">#{v}</textarea>
                                        <input type="submit" value="Edit">
                                <td>
                                    <button onclick="deleteItem(#{i})">Delet
                    <form onsubmit="newItem(document.querySelector('#new-item').value)">
                        <textarea id="new-item"></textarea>
                        <input type="submit" value="New one">
                |]
        , api =
            ApiRoutes
                { getAll = withMap_ IntMap.toList
                , getOne = \i -> maybe (throwError err404) (pure . (i,)) =<< withMap_ (IntMap.lookup i)
                , post = \v -> withMap \m ->
                    let i = maybe 1 (succ . fst) . IntMap.lookupMax $ m
                     in ((i, v), IntMap.insert i v m)
                , put = \i v -> withMap \m -> ((i, v), IntMap.insert i v m)
                , deleteOne = \i -> maybe (throwError err404) (pure . (i,)) =<< withMap (IntMap.updateLookupWithKey (\_ _ -> Nothing) i)
                , deleteAll = withMap \m -> (IntMap.toList m, mempty)
                }
        }
  where
    withMap_ :: (MonadIO m, MonadReader State m) => (IntMap Value -> a) -> m a
    withMap_ f = fmap f . readTVarIO =<< Reader.ask
    withMap :: (MonadIO m, MonadReader State m) => (IntMap Value -> (a, IntMap Value)) -> m a
    withMap f = Reader.ask >>= atomically . flip stateTVar f

instance Accept Html where
    contentTypes _ = pure $ "text" // "html" /: ("charset", "utf-8")

instance Accept Text where
    contentTypes _ = pure $ "text" // "plain" /: ("charset", "utf-8")

instance MimeRender Html Html where
    mimeRender :: Proxy Html -> Html -> LazyByteString
    mimeRender _ = LazyByteString.fromStrict . Text.encodeUtf8 . Text.pack . renderHtml

instance MimeUnrender Text Value where
    mimeUnrender :: Proxy Text -> LazyByteString -> Either String Value
    mimeUnrender _ = Right . Aeson.String . Text.decodeUtf8 . LazyByteString.toStrict
