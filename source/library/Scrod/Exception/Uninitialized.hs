module Scrod.Exception.Uninitialized where

import qualified Control.Exception as Exception
import qualified GHC.Stack as Stack
import qualified Language.Haskell.TH as TH

newtype Uninitialized = MkUninitialized
  { value :: TH.Name
  }
  deriving (Eq, Ord, Show)

instance Exception.Exception Uninitialized

throw :: (Stack.HasCallStack) => TH.Name -> a
throw = Exception.throw . MkUninitialized
