{-# LANGUAGE OverloadedStrings #-}

module Scrod.Unstable.Type.ExportIdentifier where

import qualified Scrod.Unstable.Type.Doc as Doc
import qualified Scrod.Unstable.Type.ExportName as ExportName
import qualified Scrod.Unstable.Type.Json as Json
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
  deriving (Eq, Ord, Show)

toJson :: ExportIdentifier -> Json.Json
toJson (MkExportIdentifier n s w d) =
  Json.object
    [ ("name", ExportName.toJson n),
      ("subordinates", maybe Json.Null Subordinates.toJson s),
      ("warning", maybe Json.Null Warning.toJson w),
      ("doc", maybe Json.Null Doc.toJson d)
    ]
