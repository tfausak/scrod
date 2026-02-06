module Scrod.Ghc.Uninitialized where

import qualified Control.Monad.Catch as Exception
import qualified GHC.Stack as Stack
import qualified Language.Haskell.TH as TH
import qualified System.IO.Unsafe as Unsafe

newtype Uninitialized = MkUninitialized
  { unwrap :: TH.Name
  }
  deriving (Eq, Ord, Show)

instance Exception.Exception Uninitialized

throw :: (Stack.HasCallStack) => TH.Name -> a
throw = Unsafe.unsafePerformIO . Exception.throwM . MkUninitialized
