{-# LANGUAGE DeriveGeneric #-}

module Scrod.Core.Namespace where

import qualified GHC.Generics as Generics

-- | The namespace qualification for an identifier.
data Namespace
  = -- | @v'identifier'@ syntax
    Value
  | -- | @t'identifier'@ syntax
    Type
  deriving (Eq, Generics.Generic, Ord, Show)
