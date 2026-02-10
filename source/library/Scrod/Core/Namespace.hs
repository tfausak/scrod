module Scrod.Core.Namespace where

-- | The namespace qualification for an identifier.
data Namespace
  = -- | @v'identifier'@ syntax
    Value
  | -- | @t'identifier'@ syntax
    Type
  deriving (Eq, Ord, Show)
