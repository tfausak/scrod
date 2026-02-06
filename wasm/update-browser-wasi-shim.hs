#!/usr/bin/env cabal
{- cabal:
build-depends:
  , base ^>=4.22.0
  , directory ^>=1.3.9
  , filepath ^>=1.5.4
  , process ^>=1.6.25
ghc-options: -Wall
-}

import qualified Control.Monad as Monad
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
  let dest = FilePath.joinPath ["wasm", "www", "vendor", "browser_wasi_shim"]
  Directory.createDirectoryIfMissing True dest
  entries <- Directory.listDirectory dest
  Monad.forM_ entries $ \entry ->
    Monad.when ("js" `FilePath.isExtensionOf` entry) $
      Directory.removeFile (FilePath.combine dest entry)
  Process.callProcess "curl" ["--fail", "--silent", "--show-error", "--location", url, "--output", FilePath.combine dest "index.js"]
  putStrLn $ "Updated browser_wasi_shim to " <> ref
