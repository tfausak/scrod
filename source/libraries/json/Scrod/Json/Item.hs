{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Scrod.Json.Item where

import qualified Data.Aeson as Aeson
import qualified Scrod.Json.Doc as Doc
import qualified Scrod.Json.Helpers as Helpers
import qualified Scrod.Json.ItemKey as ItemKey
import qualified Scrod.Json.ItemName as ItemName
import qualified Scrod.Type.Item as Type

fromJson :: Aeson.Value -> Either String Type.Item
fromJson = \case
  Aeson.Object obj -> do
    keyJson <- Helpers.lookupField obj "key"
    k <- ItemKey.fromJson keyJson
    parentKeyJson <- Helpers.lookupField obj "parentKey"
    pk <- case parentKeyJson of
      Aeson.Null -> Right Nothing
      _ -> fmap Just (ItemKey.fromJson parentKeyJson)
    nameJson <- Helpers.lookupField obj "name"
    n <- case nameJson of
      Aeson.Null -> Right Nothing
      _ -> fmap Just (ItemName.fromJson nameJson)
    docJson <- Helpers.lookupField obj "documentation"
    doc <- Doc.fromJson docJson
    Right $ Type.MkItem {Type.key = k, Type.parentKey = pk, Type.name = n, Type.documentation = doc}
  _ -> Left "Item must be an object"

toJson :: Type.Item -> Aeson.Value
toJson (Type.MkItem k pk n doc) =
  Aeson.object
    [ "key" Aeson..= ItemKey.toJson k,
      "parentKey" Aeson..= maybe Aeson.Null ItemKey.toJson pk,
      "name" Aeson..= maybe Aeson.Null ItemName.toJson n,
      "documentation" Aeson..= Doc.toJson doc
    ]
