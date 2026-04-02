module Main where

import Config (Config (..), getConfig)
import Control.Lens.Operators ((&), (.~), (^.))
import Control.Monad ((>=>))
import Control.Monad.Extra (whileM)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Effectful
import Effectful.FileSystem (createDirectoryIfMissing, setCurrentDirectory)
import Effectful.State.Static.Local (State, evalState)
import Itinerary (Itinerary)
import Itinerary qualified
import System.FilePath (takeDirectory)
import Test.Dramaturge hiding (Config)
import Text.URI (URI)
import Text.URI qualified as URI
import Text.URI.Lens (uriFragment, uriPath, uriQuery)
import Text.URI.QQ qualified as URI
import Prelude hiding (writeFile)

main :: IO ()
main = do
    Config{..} <- getConfig
    runEff
        . evalState Itinerary.empty
        . runDramaturge dramaturge
        $ do
            createDirectoryIfMissing True output
            setCurrentDirectory output
            newSession
            mapM_ Itinerary.push uris
            whileM step

type Tourist es =
    ( State Itinerary :> es
    , Marionette :> es
    , Concurrent :> es
    , Timeout :> es
    , Log :> es
    , FileSystem :> es
    , Hspec :> es
    )

visit :: (Tourist es) => URI -> Eff es ()
visit uri = do
    Itinerary.push uri
    whileM step

step :: (Tourist es) => Eff es Bool
step =
    Itinerary.pop >>= \case
        Nothing -> pure False
        Just url -> do
            process url
            pure True

process :: (Tourist es) => URI -> Eff es ()
process uri = do
    logInfo_ $ "Visiting " <> URI.render uri
    uri `shouldSatisfy` URI.isPathAbsolute
    let filename =
            Text.unpack . URI.render $
                URI.emptyURI & uriPath .~ (uri ^. uriPath) <> pure [URI.pathPiece|index.html|]
    navigate $ URI.render uri
    waitForElement $ ByCSS "body > *"
    logInfo_ $ "Writing " <> Text.pack filename
    createDirectoryIfMissing True . takeDirectory $ filename
    writeFile filename . Text.encodeUtf8 =<< getPageSource
    findElements (ByTag "a")
        >>= mapM_ @[]
            ( getElementAttribute "href" >=> \case
                Just href | isLocalPage href -> do
                    let href' = fromMaybe href $ Text.stripPrefix "/" href
                    uri' <-
                        URI.mkURI $
                            URI.render (uri & uriPath .~ [] & uriQuery .~ [] & uriFragment .~ Nothing)
                                <> "/"
                                <> href'
                    Itinerary.push uri'
                _ -> pure ()
            )

isLocalPage :: Text -> Bool
isLocalPage t =
    not $
        Text.null t
            || Text.isInfixOf "//" t
            || Text.isInfixOf ":" t
            || Text.isPrefixOf "#" t
