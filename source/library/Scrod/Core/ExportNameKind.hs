{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

module Scrod.Core.ExportNameKind where

import qualified GHC.Generics as Generics
import Scrod.Json.ToJson (ToJson)
import Scrod.Schema (ToSchema)

-- | Namespace annotation for a name in an export list.
data ExportNameKind
  = -- | @pattern X@
    Pattern
  | -- | @type (:+:)@
    Type
  | -- | @module Data.List@
    Module
  deriving (Eq, Generics.Generic, Ord, Show)

deriving via Generics.Generically ExportNameKind instance ToJson ExportNameKind

deriving via Generics.Generically ExportNameKind instance ToSchema ExportNameKind
