{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

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

deriving via Generics.Generically ExportName instance ToJson.ToJson ExportName

deriving via Generics.Generically ExportName instance Schema.ToSchema ExportName
