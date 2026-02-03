import qualified Control.Monad.Trans.Writer as Writer
import qualified LegendaryChainsaw
import qualified LegendaryChainsaw.Spec as Spec
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as Unit

main :: IO ()
main = Tasty.defaultMain testTree

testTree :: Tasty.TestTree
testTree = Tasty.testGroup "legendary-chainsaw" . Writer.execWriter $ LegendaryChainsaw.testSuite tasty

tasty :: Spec.Spec IO (Writer.Writer [Tasty.TestTree])
tasty = Spec.MkSpec
  { Spec.assertFailure = Unit.assertFailure
  , Spec.describe = \ s -> Writer.tell . pure . Tasty.testGroup s . Writer.execWriter
  , Spec.it = \ s -> Writer.tell . pure . Unit.testCase s
  }
