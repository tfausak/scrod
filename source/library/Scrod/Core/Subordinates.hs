{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

module Scrod.Core.Subordinates where

import qualified GHC.Generics as Generics
import qualified Scrod.Core.ExportName as ExportName
import Scrod.Json.ToJson (ToJson)
import Scrod.Schema (ToSchema)

-- | Subordinate exports for a type or class.
data Subordinates = MkSubordinates
  { -- | Whether a @(..)@ wildcard is present.
    wildcard :: Bool,
    -- | Explicitly listed children.
    explicit :: [ExportName.ExportName]
  }
  deriving (Eq, Generics.Generic, Ord, Show)

deriving via Generics.Generically Subordinates instance ToJson Subordinates

deriving via Generics.Generically Subordinates instance ToSchema Subordinates
