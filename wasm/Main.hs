module Main where

import qualified GHC.Wasm.Prim as Wasm
import qualified Scrod.Executable.Format as Format
import qualified Scrod.Executable.Main as Executable
import qualified Scrod.Extra.Builder as Builder

main :: IO ()
main = Executable.defaultMain

foreign export javascript "cliMain"
  cliMain :: IO ()

cliMain :: IO ()
cliMain = main

foreign export javascript "processHaskell"
  processHaskell :: Wasm.JSString -> Wasm.JSString -> IO Wasm.JSString

processHaskell :: Wasm.JSString -> Wasm.JSString -> IO Wasm.JSString
processHaskell formatStr input = do
  let formatString = Wasm.fromJSString formatStr
      source = Wasm.fromJSString input
  format <- case formatString of
    "json" -> pure Format.Json
    _ -> pure Format.Html
  result <- either fail pure $ Executable.runPipeline format source
  pure . Wasm.toJSString . Builder.toString $ result
