{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

module Scrod.Core.ExportIdentifier where

import qualified GHC.Generics as Generics
import qualified Scrod.Core.Doc as Doc
import qualified Scrod.Core.ExportName as ExportName
import qualified Scrod.Core.Subordinates as Subordinates
import qualified Scrod.Core.Warning as Warning
import Scrod.Json.ToJson (ToJson)
import Scrod.Schema (ToSchema)

-- | A named export: variable, type/class, or module re-export.
data ExportIdentifier = MkExportIdentifier
  { name :: ExportName.ExportName,
    subordinates :: Maybe Subordinates.Subordinates,
    warning :: Maybe Warning.Warning,
    doc :: Maybe Doc.Doc
  }
  deriving (Eq, Generics.Generic, Ord, Show)

deriving via Generics.Generically ExportIdentifier instance ToJson ExportIdentifier

deriving via Generics.Generically ExportIdentifier instance ToSchema ExportIdentifier
