module Scrod.Executable.Main where

import qualified Control.Monad as Monad
import qualified Data.ByteString.Builder as Builder
import qualified Data.Version as Version
import qualified GHC.Stack as Stack
import qualified Scrod.Convert.FromGhc as FromGhc
import qualified Scrod.Convert.ToHtml as ToHtml
import qualified Scrod.Convert.ToJson as ToJson
import qualified Scrod.Executable.Config as Config
import qualified Scrod.Executable.Flag as Flag
import qualified Scrod.Executable.Format as Format
import qualified Scrod.Executable.Style as ExecStyle
import qualified Scrod.Ghc.Parse as Parse
import qualified Scrod.Ghc.Unlit as Unlit
import qualified Scrod.Json.Value as Json
import qualified Scrod.Version as Version
import qualified Scrod.Xml.Document as Xml
import qualified System.Console.GetOpt as GetOpt
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.IO as IO

defaultMain :: (Stack.HasCallStack) => IO ()
defaultMain = do
  name <- Environment.getProgName
  arguments <- Environment.getArgs
  mainWith name arguments

mainWith :: (Stack.HasCallStack) => String -> [String] -> IO ()
mainWith name arguments = do
  flags <- Flag.fromArguments arguments
  config <- Config.fromFlags flags
  Monad.when (Config.help config) $ do
    putStr $ GetOpt.usageInfo name Flag.optDescrs
    Exit.exitSuccess
  Monad.when (Config.version config) $ do
    putStrLn $ Version.showVersion Version.version
    Exit.exitSuccess
  input <- getContents
  let preprocessed = case Config.style config of
        ExecStyle.None -> input
        ExecStyle.Bird -> Unlit.unlit Unlit.Bird input
        ExecStyle.Latex -> Unlit.unlit Unlit.Latex input
  result <- either fail pure $ Parse.parse preprocessed
  module_ <- either fail pure $ FromGhc.fromGhc result
  let encoder = case Config.format config of
        Format.Json -> Json.encode . ToJson.toJson
        Format.Html -> Xml.encode . ToHtml.toHtml
  Builder.hPutBuilder IO.stdout
    . (<> Builder.charUtf8 '\n')
    $ encoder module_
