import qualified Control.Monad.Trans.Writer.CPS as Writer
import qualified Data.List as List
import qualified Heck
import qualified Scrod.Unstable.Spec as Scrod
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as Unit

main :: IO ()
main = Tasty.defaultMain testTree

testTree :: Tasty.TestTree
testTree = Tasty.testGroup "scrod" . Writer.execWriter $ Scrod.spec heck

heck :: Heck.Test IO (Writer.Writer [Tasty.TestTree])
heck =
  Heck.MkTest
    { Heck.assertFailure = Unit.assertFailure,
      Heck.describe = \x -> Writer.tell . List.singleton . Tasty.testGroup x . Writer.execWriter,
      Heck.it = \x -> Writer.tell . List.singleton . Unit.testCase x
    }
