{-# LANGUAGE DeriveGeneric #-}

module Scrod.Unstable.Type.Export where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified GHC.Generics as Generics
import qualified Scrod.Unstable.Type.Doc as Doc
import qualified Scrod.Unstable.Type.ExportIdentifier as ExportIdentifier
import qualified Scrod.Unstable.Type.JsonOptions as JsonOptions
import qualified Scrod.Unstable.Type.Section as Section

-- | A single entry in a module's export list.
-- Mirrors GHC's IE type but simplified.
data Export
  = -- | A named export: variable, type/class, or module re-export.
    Identifier ExportIdentifier.ExportIdentifier
  | -- | Section heading: @-- * Section@
    Group Section.Section
  | -- | Inline documentation: @-- | Some doc@
    Doc Doc.Doc
  | -- | Named doc reference: @-- $chunkName@
    DocNamed Text.Text
  deriving (Eq, Ord, Show, Generics.Generic)

instance Aeson.FromJSON Export where
  parseJSON = Aeson.genericParseJSON JsonOptions.sumOptions

instance Aeson.ToJSON Export where
  toJSON = Aeson.genericToJSON JsonOptions.sumOptions
