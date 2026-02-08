import qualified GHC.Wasm.Prim as Wasm
import qualified Scrod
import qualified Scrod.Extra.Builder as Builder

main :: IO ()
main = error "required by ghc but ignored by wasm"

foreign export javascript "processHaskell"
  processHaskell :: Wasm.JSString -> Wasm.JSString -> IO Wasm.JSString

processHaskell :: Wasm.JSString -> Wasm.JSString -> IO Wasm.JSString
processHaskell formatStr input = do
  result <-
    Scrod.mainWith "scrod-wasm" ["--format", Wasm.fromJSString formatStr]
      . pure
      $ Wasm.fromJSString input
  either fail (pure . Wasm.toJSString . Builder.toString) result
