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
import qualified Data.List as List
import qualified System.Directory as Directory
import qualified System.Exit as Exit
import qualified System.FilePath as FilePath
import qualified System.IO as IO
import qualified System.Process as Process

main :: IO ()
main = do
  putStrLn "Building WASM executable..."
  Process.callProcess
    "wasm32-wasi-cabal"
    ["build", "--project-file=wasm/cabal.project", "exe:scrod-wasm"]

  putStrLn "Locating WASM binary..."
  wasm <- trim <$> Process.readProcess "find" ["dist-newstyle", "-name", "scrod-wasm.wasm", "-type", "f"] ""
  Monad.when (null wasm) $ do
    IO.hPutStrLn IO.stderr "Error: could not find scrod-wasm.wasm"
    Exit.exitFailure
  putStrLn $ "Found: " <> wasm

  putStrLn "Assembling dist directory..."
  let dist = FilePath.joinPath ["wasm", "dist"]
  Directory.createDirectoryIfMissing True dist

  putStrLn "Running GHC JS post-linker..."
  libdir <- trim <$> Process.readProcess "wasm32-wasi-ghc" ["--print-libdir"] ""
  Process.callProcess
    (FilePath.combine libdir "post-link.mjs")
    ["--input", wasm, "--output", FilePath.combine dist "ghc_wasm_jsffi.js"]

  Directory.copyFile wasm (FilePath.combine dist "scrod-wasm.wasm")

  wasmStrip <- Directory.findExecutable "wasm-strip"
  Monad.forM_ wasmStrip $ \exe -> do
    putStrLn "Running wasm-strip..."
    Process.callProcess exe [FilePath.combine dist "scrod-wasm.wasm"]

  Process.callProcess "cp" ["--recursive", "wasm/www/.", dist]

  putStrLn "Build complete."
  putStrLn "Serve with: python3 -m http.server -d wasm/dist"

trim :: String -> String
trim = List.dropWhileEnd (== '\n')
