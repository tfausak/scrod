{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Scrod.Unstable.Type.ExportIdentifier where

import qualified Data.Aeson as Aeson
import qualified Scrod.Unstable.Type.Doc as Doc
import qualified Scrod.Unstable.Type.ExportName as ExportName
import qualified Scrod.Unstable.Type.JsonHelpers as JsonHelpers
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

fromJson :: Aeson.Value -> Either String ExportIdentifier
fromJson = \case
  Aeson.Object obj -> do
    nameJson <- JsonHelpers.lookupField obj "name"
    n <- ExportName.fromJson nameJson
    subsJson <- JsonHelpers.lookupField obj "subordinates"
    subs <- case subsJson of
      Aeson.Null -> Right Nothing
      _ -> fmap Just (Subordinates.fromJson subsJson)
    warnJson <- JsonHelpers.lookupField obj "warning"
    warn <- case warnJson of
      Aeson.Null -> Right Nothing
      _ -> fmap Just (Warning.fromJson warnJson)
    docJson <- JsonHelpers.lookupField obj "doc"
    d <- case docJson of
      Aeson.Null -> Right Nothing
      _ -> fmap Just (Doc.fromJson docJson)
    Right $ MkExportIdentifier {name = n, subordinates = subs, warning = warn, doc = d}
  _ -> Left "ExportIdentifier must be an object"

toJson :: ExportIdentifier -> Aeson.Value
toJson (MkExportIdentifier n subs warn d) =
  Aeson.object
    [ "name" Aeson..= ExportName.toJson n,
      "subordinates" Aeson..= maybe Aeson.Null Subordinates.toJson subs,
      "warning" Aeson..= maybe Aeson.Null Warning.toJson warn,
      "doc" Aeson..= maybe Aeson.Null Doc.toJson d
    ]
