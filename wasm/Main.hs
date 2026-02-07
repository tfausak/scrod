module Main where

import qualified GHC.Wasm.Prim as Wasm
import qualified Scrod.Convert.FromGhc as FromGhc
import qualified Scrod.Convert.ToHtml as ToHtml
import qualified Scrod.Convert.ToJson as ToJson
import qualified Scrod.Executable.Main as Executable
import qualified Scrod.Extra.Builder as Builder
import qualified Scrod.Ghc.Parse as Parse
import qualified Scrod.Json.Value as Json
import qualified Scrod.Xml.Document as Xml

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
  let format = Wasm.fromJSString formatStr
      source = Wasm.fromJSString input
  case pipeline format source of
    Left err -> fail err
    Right out -> pure $ Wasm.toJSString out

pipeline :: String -> String -> Either String String
pipeline format source = do
  result <- Parse.parse source
  module_ <- FromGhc.fromGhc result
  pure . Builder.toString $ case format of
    "json" -> Json.encode $ ToJson.toJson module_
    _ -> Xml.encode $ ToHtml.toHtml module_
