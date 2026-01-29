import qualified Control.Monad.Trans.Writer.CPS as Writer
import qualified Data.List as List
import qualified Heck
import qualified Scrod.Unstable.JsonSpec as JsonSpec
import qualified Scrod.Unstable.Spec as Scrod
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as Unit

main :: IO ()
main = do
  -- Discover JSON test files at runtime
  testGroups <- JsonSpec.discoverTests "test-data"
  -- Build the test tree
  let jsonTestTree = Writer.execWriter $ JsonSpec.buildJsonSpec heck testGroups
  let specTestTree = Writer.execWriter $ Scrod.spec heck
  let jsonSpecTestTree = Writer.execWriter $ Scrod.jsonSpec heck
  let testTree = Tasty.testGroup "scrod" $ specTestTree <> jsonSpecTestTree <> jsonTestTree
  Tasty.defaultMain testTree

heck :: Heck.Test IO (Writer.Writer [Tasty.TestTree])
heck =
  Heck.MkTest
    { Heck.assertFailure = Unit.assertFailure,
      Heck.describe = \x -> Writer.tell . List.singleton . Tasty.testGroup x . Writer.execWriter,
      Heck.it = \x -> Writer.tell . List.singleton . Unit.testCase x
    }
