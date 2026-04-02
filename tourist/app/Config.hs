module Config where

import Control.Exception (SomeException)
import Control.Monad.Catch (try)
import Data.Bifunctor (first)
import Data.Default (Default (def))
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import Data.Version (showVersion)
import GHC.Generics (Generic)
import Options.Applicative
import Paths_tourist qualified
import System.IO.Unsafe (unsafePerformIO)
import Test.Dramaturge qualified as Dramaturge
import Test.Dramaturge.Firefox qualified as Firefox
import Test.Dramaturge.Log (LogLevel (..))
import Text.URI (URI, mkURI)
import Prelude

data Config = Config
    { dramaturge :: Dramaturge.Config
    , output :: FilePath
    , uris :: [URI]
    }
    deriving stock (Generic)

instance Default Config where
    def =
        Config
            { dramaturge = def
            , output = "."
            , uris = []
            }

parseFirefox :: Parser Firefox.Config
parseFirefox = do
    headless <-
        fromMaybe (Firefox.headless def)
            <$> optional
                ( flag' True (long "headless" <> help "Run without GUI")
                    <|> flag' False (long "graphical" <> help "Run with GUI")
                )
    program <-
        fromMaybe (Firefox.program def)
            <$> optional
                ( strOption $
                    long "firefox"
                        <> metavar "FILE"
                        <> help "Path to the Firefox binary (default: firefox)"
                )
    pure def{Firefox.headless, Firefox.program}

parseDramaturge :: Parser Dramaturge.Config
parseDramaturge = do
    firefox <- parseFirefox
    logLevel <-
        fromMaybe (Dramaturge.logLevel def)
            <$> optional (logLevelQuiet <|> logLevelVerbose <|> logLevelDebug)
    pure def{Dramaturge.firefox, Dramaturge.logLevel}
  where
    logLevelQuiet, logLevelVerbose, logLevelDebug :: Parser LogLevel
    logLevelQuiet =
        flag' LogAttention $ long "quiet" <> help "Decrease the logging verbosity level"
    logLevelVerbose = flag' LogInfo $ long "verbose" <> help "Default logging verbosity level"
    logLevelDebug =
        flag' LogTrace $
            long "debug" <> help "Increase the logging verbosity level and add stacktraces"

uriParser :: Parser URI
uriParser = argument uriReader $ metavar "URI" <> help "The sites to visit"

uriReader :: ReadM URI
uriReader =
    eitherReader $
        first (show @SomeException) . unsafePerformIO . try . mkURI . Text.pack

parseArgs :: Parser Config
parseArgs = do
    dramaturge <- parseDramaturge
    output <-
        fromMaybe (output def)
            <$> optional
                ( strOption $
                    long "output"
                        <> metavar "DIR"
                        <> help "Where to write downloaded files (default: .)"
                )
    uris <- many uriParser
    simpleVersioner $ " tourist " <> showVersion Paths_tourist.version
    pure Config{..}

parserInfo :: ParserInfo Config
parserInfo = info (helper <*> parseArgs) (fullDesc <> progDesc "tourist")

getConfig :: IO Config
getConfig = execParser parserInfo
