{-# LANGUAGE DeriveGeneric #-}

module Scrod.Core.ExportNameKind where

import qualified GHC.Generics as Generics

-- | Namespace annotation for a name in an export list.
data ExportNameKind
  = -- | @pattern X@
    Pattern
  | -- | @type (:+:)@
    Type
  | -- | @module Data.List@
    Module
  deriving (Eq, Generics.Generic, Ord, Show)
