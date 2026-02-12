import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Scrod.Executable.Main as Main
import qualified Test.Tasty.Bench as Bench

main :: IO ()
main =
  Bench.defaultMain
    [ Bench.bench "empty input" . Bench.nfIO $ run
    ]

run :: IO LazyByteString.ByteString
run = do
  result <- Main.mainWith "bench" [] (pure "")
  case result of
    Left e -> fail e
    Right b -> pure $ Builder.toLazyByteString b
