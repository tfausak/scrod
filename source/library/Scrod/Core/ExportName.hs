{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Scrod.Core.ExportName where

import qualified Data.Text as Text
import qualified GHC.Generics as Generics
import qualified Scrod.Core.ExportNameKind as ExportNameKind
import qualified Scrod.Json.ToJson as ToJson
import qualified Scrod.Schema as Schema

-- | A name in an export list, possibly annotated with @pattern@ or @type@.
data ExportName = MkExportName
  { kind :: Maybe ExportNameKind.ExportNameKind,
    name :: Text.Text
  }
  deriving (Eq, Generics.Generic, Ord, Show)
  deriving (ToJson.ToJson, Schema.ToSchema) via Generics.Generically ExportName
