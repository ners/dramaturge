module Config where

import Control.Exception (SomeException)
import Control.Lens.Regex.Text (Regex)
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
import Test.Dramaturge (Selector (..))
import Test.Dramaturge qualified as Dramaturge
import Test.Dramaturge.Firefox qualified as Firefox
import Test.Dramaturge.Log (LogLevel (..))
import Text.Regex.PCRE.Heavy (re)
import Text.Regex.PCRE.Light.Char8 qualified as PCRE
import Text.URI (URI, mkURI)
import Prelude

data Config = Config
    { dramaturge :: Dramaturge.Config
    , output :: FilePath
    , waitFor :: Selector
    , filter :: Regex
    , uris :: [URI]
    }
    deriving stock (Generic)

instance Default Config where
    def =
        Config
            { dramaturge = def
            , output = "."
            , waitFor = ByCSS "body > *"
            , filter = [re|^(?!\/\/)[^:\s]+$|]
            , uris = []
            }

parseFirefox :: Parser Firefox.Config
parseFirefox = do
    headless <-
        fromMaybe defaultFirefox.headless
            <$> optional
                ( flag' True (long "headless" <> help "Run without GUI")
                    <|> flag' False (long "graphical" <> help "Run with GUI")
                )
    program <-
        fromMaybe defaultFirefox.program
            <$> optional
                ( strOption $
                    long "firefox"
                        <> metavar "FILE"
                        <> help "Path to the Firefox binary"
                        <> value defaultFirefox.program
                        <> showDefault
                )
    pure def{Firefox.headless, Firefox.program}
  where
    defaultFirefox = (Dramaturge.firefox . dramaturge) def

parseDramaturge :: Parser Dramaturge.Config
parseDramaturge = do
    firefox <- parseFirefox
    logLevel <-
        fromMaybe defaultDramaturge.logLevel
            <$> optional (logLevelQuiet <|> logLevelVerbose <|> logLevelDebug)
    pure def{Dramaturge.firefox, Dramaturge.logLevel}
  where
    defaultDramaturge = dramaturge def
    logLevelQuiet, logLevelVerbose, logLevelDebug :: Parser LogLevel
    logLevelQuiet =
        flag' LogAttention $ long "quiet" <> help "Decrease the logging verbosity level"
    logLevelVerbose = flag' LogInfo $ long "verbose" <> help "Default logging verbosity level"
    logLevelDebug =
        flag' LogTrace $
            long "debug" <> help "Increase the logging verbosity level and add stacktraces"

uriReader :: ReadM URI
uriReader =
    eitherReader $
        first (show @SomeException) . unsafePerformIO . try . mkURI . Text.pack

regexReader :: ReadM Regex
regexReader = eitherReader $ flip PCRE.compileM [PCRE.utf8]

selectorReader :: ReadM Selector
selectorReader = ByCSS <$> str

parseConfig :: Parser Config
parseConfig = do
    dramaturge <- parseDramaturge
    output <-
        fromMaybe defaultConfig.output
            <$> optional
                ( strOption $
                    long "output"
                        <> metavar "DIR"
                        <> help "Where to write downloaded files"
                        <> value defaultConfig.output
                        <> showDefault
                )
    waitFor <-
        fromMaybe defaultConfig.waitFor
            <$> optional
                ( option selectorReader $
                    long "wait-for"
                        <> metavar "SELECTOR"
                        <> help "The selector for the element to wait for"
                        <> value defaultConfig.waitFor
                        <> showDefaultWith (unwords . drop 1 . words . show)
                )
    filter <-
        fromMaybe defaultConfig.filter
            <$> optional
                ( option regexReader $
                    long "filter"
                        <> metavar "REGEX"
                        <> help "Only follow links matching this pattern"
                        <> value defaultConfig.filter
                        <> showDefaultWith (unwords . drop 2 . words . show)
                )
    uris <- many . argument uriReader $ metavar "URI" <> help "The sites to visit"
    simpleVersioner $ " tourist " <> showVersion Paths_tourist.version
    pure Config{..}
  where
    defaultConfig :: Config
    defaultConfig = def

parserInfo :: ParserInfo Config
parserInfo = info (helper <*> parseConfig) (fullDesc <> progDesc "tourist")

getConfig :: IO Config
getConfig = execParser parserInfo
