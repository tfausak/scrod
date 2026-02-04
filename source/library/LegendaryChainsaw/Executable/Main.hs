module LegendaryChainsaw.Executable.Main where

import qualified Control.Monad as Monad
import qualified Data.Version as Version
import qualified GHC.Stack as Stack
import qualified LegendaryChainsaw.Executable.Config as Config
import qualified LegendaryChainsaw.Executable.Flag as Flag
import qualified LegendaryChainsaw.Version as Version
import qualified System.Console.GetOpt as GetOpt
import qualified System.Environment as Environment
import qualified System.Exit as Exit

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
  putStrLn "TODO"
