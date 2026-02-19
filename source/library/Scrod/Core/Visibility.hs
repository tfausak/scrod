{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Scrod.Core.Visibility where

import qualified GHC.Generics as Generics
import qualified Scrod.Json.ToJson as ToJson
import qualified Scrod.Schema as Schema

-- | The visibility of an item with respect to the module's export list.
data Visibility
  = -- | Directly named in the export list (including subordinates named
    -- explicitly like @Foo(Bar, Baz)@), or every top-level item when
    -- there is no export list.
    Exported
  | -- | Visible because a parent is exported with a @(..)@ wildcard
    -- (e.g. constructors via @Foo(..)@, class methods, record fields).
    Wildcard
  | -- | Always visible regardless of the export list (class instances,
    -- standalone deriving, derived instances, rules, defaults,
    -- annotations, splices).
    Implicit
  | -- | Not in the export list.
    Unexported
  deriving (Eq, Generics.Generic, Ord, Show)
  deriving (ToJson.ToJson, Schema.ToSchema) via Generics.Generically Visibility
