module Scrod.Unstable.Main where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Options.Applicative as Options
import qualified Scrod.Unstable.Convert as Convert
import qualified Scrod.Unstable.Extra.Http as Http
import qualified Scrod.Unstable.Parse as Parse
import qualified Scrod.Unstable.Type.Html as Html
import qualified Scrod.Unstable.Type.HtmlInterface as HtmlInterface
import qualified Scrod.Unstable.Type.InputFormat as InputFormat
import qualified Scrod.Unstable.Type.Interface as Interface
import qualified Scrod.Unstable.Type.OutputFormat as OutputFormat
import qualified System.Exit as Exit
import qualified System.IO as IO

data Options = MkOptions
  { optInputFormat :: InputFormat.InputFormat,
    optOutputFormat :: OutputFormat.OutputFormat,
    optUrl :: Maybe Text.Text,
    optExtensions :: [String]
  }
  deriving (Eq, Ord, Show)

inputFormatOption :: Options.Parser InputFormat.InputFormat
inputFormatOption =
  Options.option readFormat $
    Options.short 'i'
      <> Options.long "input"
      <> Options.metavar "FORMAT"
      <> Options.value InputFormat.Haskell
      <> Options.help "Input format: haskell (default) or json"
  where
    readFormat :: Options.ReadM InputFormat.InputFormat
    readFormat = Options.eitherReader $ \s -> case s of
      "haskell" -> Right InputFormat.Haskell
      "json" -> Right InputFormat.Json
      _ -> Left $ "Invalid input format: " <> s <> ". Expected 'haskell' or 'json'."

outputFormatOption :: Options.Parser OutputFormat.OutputFormat
outputFormatOption =
  Options.option readFormat $
    Options.short 'o'
      <> Options.long "output"
      <> Options.metavar "FORMAT"
      <> Options.value OutputFormat.Json
      <> Options.help "Output format: json (default) or html"
  where
    readFormat :: Options.ReadM OutputFormat.OutputFormat
    readFormat = Options.eitherReader $ \s -> case s of
      "json" -> Right OutputFormat.Json
      "html" -> Right OutputFormat.Html
      _ -> Left $ "Invalid output format: " <> s <> ". Expected 'json' or 'html'."

urlOption :: Options.Parser (Maybe Text.Text)
urlOption =
  Options.optional . Options.strOption $
    Options.long "url"
      <> Options.short 'u'
      <> Options.metavar "URL"
      <> Options.help "Fetch Haskell source from URL"

extensionOption :: Options.Parser [String]
extensionOption =
  Options.many . Options.strOption $
    Options.short 'X'
      <> Options.metavar "EXTENSION"
      <> Options.help "Enable GHC extension (e.g., OverloadedStrings)"

optionsParser :: Options.Parser Options
optionsParser =
  MkOptions
    <$> inputFormatOption
    <*> outputFormatOption
    <*> urlOption
    <*> extensionOption

parserInfo :: Options.ParserInfo Options
parserInfo =
  Options.info (Options.helper <*> optionsParser) $
    Options.fullDesc
      <> Options.header "scrod - Haskell documentation extraction tool"
      <> Options.progDesc "Reads input from stdin, outputs documentation to stdout."

defaultMain :: IO ()
defaultMain = do
  opts <- Options.execParser parserInfo
  contentsResult <- case optUrl opts of
    Nothing -> Right <$> getContents
    Just url -> Http.fetch url
  case contentsResult of
    Left err -> do
      IO.hPutStrLn IO.stderr err
      Exit.exitFailure
    Right contents ->
      case parseInput (optInputFormat opts) (optExtensions opts) contents of
        Left err -> do
          IO.hPutStrLn IO.stderr err
          Exit.exitFailure
        Right interface -> do
          let output = renderOutput (optOutputFormat opts) interface
          LazyByteString.putStr output
          putStrLn ""

parseInput :: InputFormat.InputFormat -> [String] -> String -> Either String Interface.Interface
parseInput format extensions contents = case format of
  InputFormat.Haskell -> extract extensions contents
  InputFormat.Json -> parseJson contents

parseJson :: String -> Either String Interface.Interface
parseJson =
  Aeson.eitherDecodeStrict'
    . Encoding.encodeUtf8
    . Text.pack

renderOutput :: OutputFormat.OutputFormat -> Interface.Interface -> LazyByteString.ByteString
renderOutput format interface = case format of
  OutputFormat.Json -> Aeson.encode interface
  OutputFormat.Html -> Html.render $ HtmlInterface.toHtml interface

extract :: [String] -> String -> Either String Interface.Interface
extract extensions = Convert.convert extensions . Parse.parse
