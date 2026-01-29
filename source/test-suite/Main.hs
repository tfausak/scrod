import qualified Control.Monad.Trans.Writer.CPS as Writer
import qualified Data.List as List
import qualified Heck
import qualified JsonSpec
import qualified Spec
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as Unit

main :: IO ()
main = do
  -- Discover JSON test files at runtime
  testGroups <- JsonSpec.discoverTests "test-data"
  -- Build the test tree
  let jsonTestTree = Writer.execWriter $ JsonSpec.buildJsonSpec heck testGroups
  let specTestTree = Writer.execWriter $ Spec.spec heck
  let testTree = Tasty.testGroup "scrod" $ specTestTree <> jsonTestTree
  Tasty.defaultMain testTree

heck :: Heck.Test IO (Writer.Writer [Tasty.TestTree])
heck =
  Heck.MkTest
    { Heck.assertFailure = Unit.assertFailure,
      Heck.describe = \x -> Writer.tell . List.singleton . Tasty.testGroup x . Writer.execWriter,
      Heck.it = \x -> Writer.tell . List.singleton . Unit.testCase x
    }
