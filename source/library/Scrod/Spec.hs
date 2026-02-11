{-# LANGUAGE RankNTypes #-}

-- | A test specification DSL that abstracts over the test framework.
--
-- This module defines a record-based interface ('Spec') for writing tests
-- without depending on a concrete test framework. The test-suite entry point
-- wires 'Spec' into Tasty\/HUnit, but the abstraction allows library modules
-- to define @spec@ functions that are framework-agnostic.
module Scrod.Spec where

import qualified Control.Monad as Monad
import qualified GHC.Stack as Stack
import qualified Language.Haskell.TH as TH

-- | A test specification parameterized by two monads:
--
-- * @m@ — the monad for assertions (e.g., individual test actions).
-- * @n@ — the monad for test tree structure (e.g., grouping and registration).
data Spec m n = MkSpec
  { -- | Signal a test failure with a message.
    assertFailure :: forall x. (Stack.HasCallStack) => String -> m x,
    -- | Group a set of tests under a label.
    describe :: String -> n () -> n (),
    -- | Register a single test case with a label.
    it :: String -> m () -> n ()
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
