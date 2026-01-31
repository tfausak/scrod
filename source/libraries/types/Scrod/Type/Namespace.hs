module Scrod.Type.Namespace where

-- | The namespace qualification for an identifier.
-- Mirrors 'Documentation.Haddock.Types.Namespace' from haddock-library,
-- but without the 'None' case (represented as 'Nothing' in 'Identifier').
data Namespace
  = -- | v'identifier' syntax
    Value
  | -- | t'identifier' syntax
    Type
  deriving (Eq, Ord, Show)
