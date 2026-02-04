module LegendaryChainsaw.Executable.Main where

import qualified Control.Monad as Monad
import qualified Data.ByteString.Builder as Builder
import qualified Data.Version as Version
import qualified GHC.Stack as Stack
import qualified LegendaryChainsaw.Convert.FromGhc as FromGhc
import qualified LegendaryChainsaw.Convert.ToJson as ToJson
import qualified LegendaryChainsaw.Executable.Config as Config
import qualified LegendaryChainsaw.Executable.Flag as Flag
import qualified LegendaryChainsaw.Ghc.Parse as Parse
import qualified LegendaryChainsaw.Json.Value as Value
import qualified LegendaryChainsaw.Version as Version
import qualified System.Console.GetOpt as GetOpt
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.IO as IO

executable :: (Stack.HasCallStack) => IO ()
executable = do
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
  result <- either fail pure $ Parse.parse input
  module_ <- either fail pure $ FromGhc.fromGhc result
  Builder.hPutBuilder IO.stdout
    . (<> Builder.char7 '\n')
    . Value.encode
    $ ToJson.toJson module_
