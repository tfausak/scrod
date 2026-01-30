{-# LANGUAGE DeriveGeneric #-}

module Scrod.Unstable.Type.ExportIdentifier where

import qualified Data.Aeson as Aeson
import qualified GHC.Generics as Generics
import qualified Scrod.Unstable.Type.Doc as Doc
import qualified Scrod.Unstable.Type.ExportName as ExportName
import qualified Scrod.Unstable.Type.Subordinates as Subordinates
import qualified Scrod.Unstable.Type.Warning as Warning

-- | A named export: variable, type/class, or module re-export.
-- Variables: @foo@, @pattern P@
-- Types/classes with optional subordinates: @Foo@, @Foo(..)@, @Foo(Bar, Baz)@
-- Module re-exports: @module Data.List@
-- With optional warning: @{-# WARNING "msg" #-} foo@
data ExportIdentifier = MkExportIdentifier
  { name :: ExportName.ExportName,
    subordinates :: Maybe Subordinates.Subordinates,
    warning :: Maybe Warning.Warning,
    doc :: Maybe Doc.Doc
  }
  deriving (Eq, Ord, Show, Generics.Generic)

instance Aeson.FromJSON ExportIdentifier where
  parseJSON = Aeson.genericParseJSON Aeson.defaultOptions {Aeson.fieldLabelModifier = id}

instance Aeson.ToJSON ExportIdentifier where
  toJSON = Aeson.genericToJSON Aeson.defaultOptions {Aeson.fieldLabelModifier = id}
