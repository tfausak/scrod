module Scrod.Unstable.Spec where

import qualified Heck

spec :: (Monad m, Monad n) => Heck.Test m n -> n ()
spec t = do
  Heck.describe t "" $ do
    Heck.it t "" $ do
      Heck.assertEq t (1 + 1) (2 :: Int)

    Heck.it t "" $ do
      pure ()
      Heck.assertFailure t "pending"
