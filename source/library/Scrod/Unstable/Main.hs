module Scrod.Unstable.Main where

import qualified Data.ByteString.Lazy as LazyByteString
import qualified Options.Applicative as Options
import qualified Scrod.Unstable.Convert as Convert
import qualified Scrod.Unstable.Parse as Parse
import qualified Scrod.Unstable.Type.Html as Html
import qualified Scrod.Unstable.Type.HtmlInterface as HtmlInterface
import qualified Scrod.Unstable.Type.Interface as Interface
import qualified Scrod.Unstable.Type.Json as Json
import qualified Scrod.Unstable.Type.OutputFormat as OutputFormat
import qualified System.Exit as Exit
import qualified System.IO as IO

newtype Options = MkOptions
  { optFormat :: OutputFormat.OutputFormat
  }
  deriving (Eq, Ord, Show)

formatOption :: Options.Parser OutputFormat.OutputFormat
formatOption =
  Options.option readFormat $
    Options.short 'f'
      <> Options.long "format"
      <> Options.metavar "FORMAT"
      <> Options.value OutputFormat.Json
      <> Options.help "Output format: json (default) or html"
  where
    readFormat :: Options.ReadM OutputFormat.OutputFormat
    readFormat = Options.eitherReader $ \s -> case s of
      "json" -> Right OutputFormat.Json
      "html" -> Right OutputFormat.Html
      _ -> Left $ "Invalid format: " <> s <> ". Expected 'json' or 'html'."

optionsParser :: Options.Parser Options
optionsParser = fmap MkOptions formatOption

parserInfo :: Options.ParserInfo Options
parserInfo =
  Options.info (Options.helper <*> optionsParser) $
    Options.fullDesc
      <> Options.header "scrod - Haskell documentation extraction tool"
      <> Options.progDesc "Reads Haskell source from stdin, outputs documentation to stdout."

defaultMain :: IO ()
defaultMain = do
  opts <- Options.execParser parserInfo
  contents <- getContents
  case extract contents of
    Left err -> do
      IO.hPutStrLn IO.stderr err
      Exit.exitFailure
    Right interface -> do
      let output = renderOutput (optFormat opts) interface
      LazyByteString.putStr output
      putStrLn ""

renderOutput :: OutputFormat.OutputFormat -> Interface.Interface -> LazyByteString.ByteString
renderOutput format interface = case format of
  OutputFormat.Json -> Json.render $ Interface.toJson interface
  OutputFormat.Html -> Html.render $ HtmlInterface.toHtml interface

extract :: String -> Either String Interface.Interface
extract = Convert.convert . Parse.parse
