{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

module Scrod.Core.ExportNameKind where

import qualified GHC.Generics as Generics
import qualified Scrod.Json.ToJson as ToJson
import qualified Scrod.Schema as Schema

-- | Namespace annotation for a name in an export list.
data ExportNameKind
  = -- | @pattern X@
    Pattern
  | -- | @type (:+:)@
    Type
  | -- | @module Data.List@
    Module
  deriving (Eq, Generics.Generic, Ord, Show)

deriving via Generics.Generically ExportNameKind instance ToJson.ToJson ExportNameKind

deriving via Generics.Generically ExportNameKind instance Schema.ToSchema ExportNameKind
