import qualified Control.Monad.Trans.Writer as Writer
import qualified Scrod
import qualified Scrod.Spec as Spec
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as Unit

main :: IO ()
main = Tasty.defaultMain testTree

testTree :: Tasty.TestTree
testTree = Tasty.testGroup "scrod" . Writer.execWriter $ Scrod.spec tasty

tasty :: Spec.Spec IO (Writer.Writer [Tasty.TestTree])
tasty =
  Spec.MkSpec
    { Spec.assertFailure = Unit.assertFailure,
      Spec.describe = \s -> Writer.tell . pure . Tasty.testGroup s . Writer.execWriter,
      Spec.it = \s -> Writer.tell . pure . Unit.testCase s
    }
