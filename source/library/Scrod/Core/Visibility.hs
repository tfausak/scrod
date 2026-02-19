{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Scrod.Core.Visibility where

import qualified GHC.Generics as Generics
import qualified Scrod.Json.ToJson as ToJson
import qualified Scrod.Schema as Schema

-- | The visibility of an item with respect to the module's export list.
data Visibility
  = -- | In the export list (directly named, via subordinates like
    -- @Foo(Bar)@, or via wildcard like @Foo(..)@), or every top-level
    -- item when there is no export list.
    Exported
  | -- | Always visible regardless of the export list (class instances,
    -- standalone deriving, derived instances, rules, defaults,
    -- annotations, splices).
    Implicit
  | -- | Not in the export list.
    Unexported
  deriving (Eq, Generics.Generic, Ord, Show)
  deriving (ToJson.ToJson, Schema.ToSchema) via Generics.Generically Visibility
