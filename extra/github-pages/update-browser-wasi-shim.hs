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
  ref <- Maybe.fromMaybe "2f86b49dce50916e2984029c535321e34b234229" . Maybe.listToMaybe <$> Environment.getArgs
  let url =
        "https://esm.sh/gh/haskell-wasm/browser_wasi_shim@"
          <> ref
          <> "/es2022/browser_wasi_shim.mjs"
  let dest = FilePath.joinPath ["extra", "github-pages", "vendor"]
  Directory.createDirectoryIfMissing True dest
  Process.callProcess "curl" ["--fail", "--silent", "--show-error", "--location", url, "--output", FilePath.combine dest "browser_wasi_shim.js"]
  putStrLn $ "Updated browser_wasi_shim to " <> ref
