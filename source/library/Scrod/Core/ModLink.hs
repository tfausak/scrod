{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

module Scrod.Core.ModLink where

import qualified GHC.Generics as Generics
import qualified Scrod.Core.ModuleName as ModuleName
import qualified Scrod.Json.ToJson as ToJson
import qualified Scrod.Schema as Schema

-- | A link to a module with an optional label.
data ModLink doc = MkModLink
  { name :: ModuleName.ModuleName,
    label :: Maybe doc
  }
  deriving (Eq, Generics.Generic, Ord, Show)

deriving via Generics.Generically (ModLink doc) instance (ToJson.ToJson doc) => ToJson.ToJson (ModLink doc)

deriving via Generics.Generically (ModLink doc) instance (Schema.ToSchema doc) => Schema.ToSchema (ModLink doc)
