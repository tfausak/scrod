module Scrod.Unstable.Main where

import qualified Data.ByteString.Lazy as LazyByteString
import qualified Scrod.Unstable.Convert as Convert
import qualified Scrod.Unstable.Parse as Parse
import qualified Scrod.Unstable.Type.Interface as Interface
import qualified Scrod.Unstable.Type.Json as Json
import qualified System.Exit as Exit
import qualified System.IO as IO

defaultMain :: IO ()
defaultMain = do
  contents <- getContents
  case extract contents of
    Left err -> do
      IO.hPutStrLn IO.stderr err
      Exit.exitFailure
    Right interface -> do
      LazyByteString.putStr . Json.render $ Interface.toJson interface
      putStrLn ""

extract :: String -> Either String Interface.Interface
extract = Convert.convert . Parse.parse
