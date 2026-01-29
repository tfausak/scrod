module HeckHelpers where

import qualified GHC.Stack as Stack
import qualified Heck

assertSatisfies :: (Stack.HasCallStack, Applicative m) => Heck.Test m n -> (a -> Bool) -> a -> m ()
assertSatisfies t p = Heck.assertEq t True . p

expectRight :: (Stack.HasCallStack, Applicative m, Show a, Show b) => Heck.Test m n -> Either a b -> m b
expectRight t e = case e of
  Left _ -> Heck.assertFailure t $ "expected Right but got " <> show e
  Right r -> pure r
