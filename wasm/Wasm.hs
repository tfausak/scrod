import qualified Control.Concurrent as Concurrent
import qualified Control.Monad as Monad
import qualified GHC.Wasm.Prim as Wasm
import qualified Scrod
import qualified Scrod.Extra.Builder as Builder

main :: IO ()
main = Concurrent.threadDelay maxBound

foreign export javascript "processHaskell"
  processHaskell :: Wasm.JSVal -> Wasm.JSString -> IO Wasm.JSString

foreign import javascript "((arr) => arr.length)"
  js_array_length :: Wasm.JSVal -> IO Int

foreign import javascript "((arr, i) => arr[i])"
  js_array_get :: Wasm.JSVal -> Int -> IO Wasm.JSString

processHaskell :: Wasm.JSVal -> Wasm.JSString -> IO Wasm.JSString
processHaskell argsArray input = do
  len <- js_array_length argsArray
  args <- Monad.forM [0 .. len - 1] $ \i -> do
    jsstr <- js_array_get argsArray i
    pure $ Wasm.fromJSString jsstr
  result <- Scrod.mainWith "scrod-wasm" args . pure $ Wasm.fromJSString input
  pure . Wasm.toJSString $ either id Builder.toString result
