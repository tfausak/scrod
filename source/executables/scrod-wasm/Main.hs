import qualified GHC.Wasm.Prim as Wasm
import qualified Scrod
import qualified Scrod.Extra.Builder as Builder

main :: IO ()
main = error "required by ghc but ignored by wasm"

foreign export javascript "scrod"
  scrod :: Wasm.JSVal -> Wasm.JSString -> IO Wasm.JSString

scrod :: Wasm.JSVal -> Wasm.JSString -> IO Wasm.JSString
scrod rawArguments input = do
  size <- jsArrayLength rawArguments
  arguments <- traverse (fmap Wasm.fromJSString . jsArrayIndex rawArguments) [0 .. size - 1]
  result <-
    Scrod.mainWith "scrod-wasm" arguments
      . pure
      $ Wasm.fromJSString input
  either fail (pure . Wasm.toJSString . Builder.toString) result

foreign import javascript unsafe "$1.length"
  jsArrayLength :: Wasm.JSVal -> IO Int

foreign import javascript unsafe "$1[$2]"
  jsArrayIndex :: Wasm.JSVal -> Int -> IO Wasm.JSString
