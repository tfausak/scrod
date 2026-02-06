import qualified GHC.Stack as Stack
import qualified Scrod

main :: (Stack.HasCallStack) => IO ()
main = Scrod.executable
