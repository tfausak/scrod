{-# LANGUAGE RankNTypes #-}

module LegendaryChainsaw.Spec where

import qualified Control.Monad as Monad
import qualified GHC.Stack as Stack
import qualified Language.Haskell.TH as TH

data Spec m n = MkSpec
  { assertFailure :: forall x . Stack.HasCallStack => String -> m x
  , describe :: String -> n () -> n ()
  , it :: String -> m () -> n ()
  }

assertEq :: (Stack.HasCallStack, Applicative m, Eq a, Show a) => Spec m n -> a -> a -> m ()
assertEq s x y = Monad.unless (x == y) . assertFailure s $ "expected: " <> show x <> " == " <> show y

assertNe :: (Stack.HasCallStack, Applicative m, Eq a, Show a) => Spec m n -> a -> a -> m ()
assertNe s x y = Monad.unless (x /= y) . assertFailure s $ "expected: " <> show x <> " /= " <> show y

assertLt :: (Stack.HasCallStack, Applicative m, Ord a, Show a) => Spec m n -> a -> a -> m ()
assertLt s x y = Monad.unless (x < y) . assertFailure s $ "expected: " <> show x <> " < " <> show y

assertLe :: (Stack.HasCallStack, Applicative m, Ord a, Show a) => Spec m n -> a -> a -> m ()
assertLe s x y = Monad.unless (x <= y) . assertFailure s $ "expected: " <> show x <> " <= " <> show y

assertGt :: (Stack.HasCallStack, Applicative m, Ord a, Show a) => Spec m n -> a -> a -> m ()
assertGt s x y = Monad.unless (x > y) . assertFailure s $ "expected: " <> show x <> " > " <> show y

assertGe :: (Stack.HasCallStack, Applicative m, Ord a, Show a) => Spec m n -> a -> a -> m ()
assertGe s x y = Monad.unless (x >= y) . assertFailure s $ "expected: " <> show x <> " >= " <> show y

named :: Spec m n -> TH.Name -> n () -> n ()
named s = describe s . show
