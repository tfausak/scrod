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
import qualified Data.Char as Char
import qualified Data.List as List
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import qualified System.Process as Process

main :: IO ()
main = do
  putStrLn "starting"

  putStrLn "updating"
  Process.callProcess "wasm32-wasi-cabal" ["update"]
  putStrLn "updated"

  putStrLn "building"
  Process.callProcess "wasm32-wasi-cabal" ["--project-file=wasm/cabal.project", "build", "scrod-wasm"]
  putStrLn "built"

  putStrLn "finding"
  wasm <- trim <$> Process.readProcess "wasm32-wasi-cabal" ["--project-file=wasm/cabal.project", "list-bin", "scrod-wasm"] ""
  putStrLn "found"

  putStrLn "copying assets"
  let dist = FilePath.joinPath ["wasm", "dist"]
  Directory.createDirectoryIfMissing True dist
  Process.callProcess "cp" ["--recursive", "web/.", dist]
  putStrLn "copied assets"

  putStrLn "running post linker"
  libdir <- trim <$> Process.readProcess "wasm32-wasi-ghc" ["--print-libdir"] ""
  Process.callProcess
    (FilePath.combine libdir "post-link.mjs")
    ["--input", wasm, "--output", FilePath.combine dist "ghc_wasm_jsffi.js"]
  putStrLn "ran post linker"

  putStrLn "copying wasm"
  let target = FilePath.combine dist "scrod-wasm.wasm"
  Directory.copyFile wasm target
  putStrLn "copied wasm"

  maybeWasmStrip <- Directory.findExecutable "wasm-strip"
  Monad.forM_ maybeWasmStrip $ \wasmStrip -> do
    putStrLn "stripping wasm"
    Process.callProcess wasmStrip [target]
    putStrLn "stripped wasm"

  putStrLn "done"

trim :: String -> String
trim = List.dropWhileEnd Char.isSpace . List.dropWhile Char.isSpace
