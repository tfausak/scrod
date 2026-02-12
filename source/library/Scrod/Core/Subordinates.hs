{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Scrod.Core.Subordinates where

import qualified GHC.Generics as Generics
import qualified Scrod.Core.ExportName as ExportName
import qualified Scrod.Json.ToJson as ToJson
import qualified Scrod.Schema as Schema

-- | Subordinate exports for a type or class.
data Subordinates = MkSubordinates
  { -- | Whether a @(..)@ wildcard is present.
    wildcard :: Bool,
    -- | Explicitly listed children.
    explicit :: [ExportName.ExportName]
  }
  deriving (Eq, Generics.Generic, Ord, Show)
  deriving (ToJson.ToJson, Schema.ToSchema) via Generics.Generically Subordinates
