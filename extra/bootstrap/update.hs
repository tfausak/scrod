#!/usr/bin/env cabal
{- cabal:
build-depends:
  , base ^>=4.22.0
  , directory ^>=1.3.9
  , filepath ^>=1.5.4
  , process ^>=1.6.25
ghc-options: -Wall
-}

import qualified Data.Maybe as Maybe
import qualified System.Directory as Directory
import qualified System.Environment as Environment
import qualified System.FilePath as FilePath
import qualified System.Process as Process

main :: IO ()
main = do
  version <- Maybe.fromMaybe "5.3.8" . Maybe.listToMaybe <$> Environment.getArgs
  let url =
        "https://cdn.jsdelivr.net/npm/bootstrap@"
          <> version
          <> "/dist/css/bootstrap.min.css"
  let dest = FilePath.joinPath ["extra", "bootstrap"]
  Directory.createDirectoryIfMissing True dest
  Process.callProcess "curl" ["--fail", "--silent", "--show-error", "--location", url, "--output", FilePath.combine dest "bootstrap.min.css"]
  putStrLn $ "Updated bootstrap to " <> version
