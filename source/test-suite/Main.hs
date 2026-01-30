import qualified JsonSpec
import qualified Spec
import qualified Test.Tasty as Tasty

main :: IO ()
main = do
  jsonTests <- JsonSpec.discoverTests "test-data"
  let testTree =
        Tasty.testGroup
          "scrod"
          (Spec.spec <> jsonTests)
  Tasty.defaultMain testTree
