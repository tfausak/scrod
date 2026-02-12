{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

module Scrod.Core.Export where

import qualified Data.Text as Text
import qualified GHC.Generics as Generics
import qualified Scrod.Core.Doc as Doc
import qualified Scrod.Core.ExportIdentifier as ExportIdentifier
import qualified Scrod.Core.Section as Section
import Scrod.Json.ToJson (ToJson)
import Scrod.Schema (ToSchema)

-- | A single entry in a module's export list.
data Export
  = -- | A named export: variable, type/class, or module re-export.
    Identifier ExportIdentifier.ExportIdentifier
  | -- | Section heading: @-- * Section@
    Group Section.Section
  | -- | Inline documentation: @-- | Some doc@
    Doc Doc.Doc
  | -- | Named doc reference: @-- $chunkName@
    DocNamed Text.Text
  deriving (Eq, Generics.Generic, Ord, Show)

deriving via Generics.Generically Export instance ToJson Export

deriving via Generics.Generically Export instance ToSchema Export
