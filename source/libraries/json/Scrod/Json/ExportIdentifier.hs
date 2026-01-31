{-# LANGUAGE OverloadedStrings #-}

module Scrod.Json.ExportIdentifier where

import qualified Data.Aeson as Aeson
import qualified Scrod.Json.Doc as Doc
import qualified Scrod.Json.ExportName as ExportName
import qualified Scrod.Json.Helpers as Helpers
import qualified Scrod.Json.Subordinates as Subordinates
import qualified Scrod.Json.Warning as Warning
import qualified Scrod.Type.ExportIdentifier as Type

fromJson :: Aeson.Value -> Either String Type.ExportIdentifier
fromJson value = case value of
  Aeson.Object obj -> do
    nameJson <- Helpers.lookupField obj "name"
    n <- ExportName.fromJson nameJson
    subsJson <- Helpers.lookupField obj "subordinates"
    subs <- case subsJson of
      Aeson.Null -> Right Nothing
      _ -> fmap Just (Subordinates.fromJson subsJson)
    warnJson <- Helpers.lookupField obj "warning"
    warn <- case warnJson of
      Aeson.Null -> Right Nothing
      _ -> fmap Just (Warning.fromJson warnJson)
    docJson <- Helpers.lookupField obj "doc"
    d <- case docJson of
      Aeson.Null -> Right Nothing
      _ -> fmap Just (Doc.fromJson docJson)
    Right $ Type.MkExportIdentifier {Type.name = n, Type.subordinates = subs, Type.warning = warn, Type.doc = d}
  _ -> Left "ExportIdentifier must be an object"

toJson :: Type.ExportIdentifier -> Aeson.Value
toJson (Type.MkExportIdentifier n subs warn d) =
  Aeson.object
    [ "name" Aeson..= ExportName.toJson n,
      "subordinates" Aeson..= maybe Aeson.Null Subordinates.toJson subs,
      "warning" Aeson..= maybe Aeson.Null Warning.toJson warn,
      "doc" Aeson..= maybe Aeson.Null Doc.toJson d
    ]
