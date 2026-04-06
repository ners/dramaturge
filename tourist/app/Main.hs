module Main where

import Config (Config (..), getConfig)
import Control.Lens.Combinators (has)
import Control.Lens.Operators ((&), (.~), (^.))
import Control.Lens.Regex.Text (regexing)
import Control.Monad (when, (>=>))
import Control.Monad.Extra (whileM)
import Data.Aeson qualified as Aeson
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Effectful
import Effectful.FileSystem (createDirectoryIfMissing, setCurrentDirectory)
import Effectful.Reader.Static (Reader, runReader)
import Effectful.Reader.Static qualified as Reader
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
    config <- getConfig
    runEff
        . runReader config
        . evalState Itinerary.empty
        . runDramaturge config.dramaturge
        $ do
            createDirectoryIfMissing True config.output
            setCurrentDirectory config.output
            newSession
            mapM_ Itinerary.push config.uris
            whileM step

type Tourist es =
    ( HasCallStack
    , Reader Config :> es
    , State Itinerary :> es
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
    config <- Reader.ask @Config
    logInfo_ $ "Visiting " <> URI.render uri
    uri `shouldSatisfy` URI.isPathAbsolute
    let filename =
            Text.unpack . URI.render $
                URI.emptyURI & uriPath .~ (uri ^. uriPath) <> pure [URI.pathPiece|index.html|]
    navigate $ URI.render uri
    waitForElement config.waitFor
    logInfo_ $ "Writing " <> Text.pack filename
    createDirectoryIfMissing True . takeDirectory $ filename
    writeFile filename . Text.encodeUtf8 =<< getPageSource
    findElements (ByTag "a")
        >>= mapM_ @[]
            ( getElementAttribute "href" >=> mapM_ \href -> do
                let isLocal = has (regexing config.filter) href
                logTrace "Found link" . Aeson.object $
                    [ ("page", Aeson.toJSON . URI.render $ uri)
                    , ("href", Aeson.toJSON href)
                    , ("local", Aeson.toJSON isLocal)
                    ]
                when isLocal do
                    let href' = fromMaybe href $ Text.stripPrefix "/" href
                    uri' <-
                        URI.mkURI $
                            URI.render (uri & uriPath .~ [] & uriQuery .~ [] & uriFragment .~ Nothing)
                                <> "/"
                                <> href'
                    Itinerary.push uri'
            )
