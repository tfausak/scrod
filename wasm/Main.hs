module Main where

import qualified GHC.Wasm.Prim as Wasm
import qualified LegendaryChainsaw.Convert.FromGhc as FromGhc
import qualified LegendaryChainsaw.Convert.ToHtml as ToHtml
import qualified LegendaryChainsaw.Extra.Builder as Builder
import qualified LegendaryChainsaw.Ghc.Parse as Parse
import qualified LegendaryChainsaw.Xml.Document as Xml

-- | Not called at runtime. The WASM reactor module uses -no-hs-main, but GHC
-- still requires main to be defined.
main :: IO ()
main = pure ()

foreign export javascript "processHaskell"
  processHaskell :: Wasm.JSString -> IO Wasm.JSString

processHaskell :: Wasm.JSString -> IO Wasm.JSString
processHaskell input = do
  let source = Wasm.fromJSString input
  pure . Wasm.toJSString $ case pipeline source of
    Left err -> "<pre class=\"error\">" <> escapeHtml err <> "</pre>"
    Right html -> html

pipeline :: String -> Either String String
pipeline source = do
  result <- Parse.parse source
  module_ <- FromGhc.fromGhc result
  pure
    . Builder.toString
    . Xml.encode
    $ ToHtml.toHtml module_

escapeHtml :: String -> String
escapeHtml = concatMap $ \c -> case c of
  '<' -> "&lt;"
  '>' -> "&gt;"
  '&' -> "&amp;"
  '"' -> "&quot;"
  '\'' -> "&#39;"
  _ -> [c]
