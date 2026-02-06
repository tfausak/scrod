module Scrod.Core.ExportNameKind where

-- | Namespace annotation for a name in an export list.
data ExportNameKind
  = -- | @pattern X@
    Pattern
  | -- | @type (:+:)@
    Type
  | -- | @module Data.List@
    Module
  deriving (Eq, Ord, Show)
