{-# LANGUAGE OverloadedStrings #-}

module Scrod.Json.Item where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Scrod.Json.Doc as Doc
import qualified Scrod.Json.Helpers as Helpers
import qualified Scrod.Json.ItemKey as ItemKey
import qualified Scrod.Json.ItemName as ItemName
import qualified Scrod.Type.Item as Type

fromJson :: Aeson.Value -> Either String Type.Item
fromJson value = case value of
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
    let signatureJson = KeyMap.lookup (Key.fromText "signature") obj
    sig <- case signatureJson of
      Nothing -> Right Nothing
      Just Aeson.Null -> Right Nothing
      Just (Aeson.String s) -> Right (Just s)
      _ -> Left "signature must be null or a string"
    Right $ Type.MkItem {Type.key = k, Type.parentKey = pk, Type.name = n, Type.documentation = doc, Type.signature = sig}
  _ -> Left "Item must be an object"

toJson :: Type.Item -> Aeson.Value
toJson (Type.MkItem k pk n doc sig) =
  Aeson.object
    [ "key" Aeson..= ItemKey.toJson k,
      "parentKey" Aeson..= maybe Aeson.Null ItemKey.toJson pk,
      "name" Aeson..= maybe Aeson.Null ItemName.toJson n,
      "documentation" Aeson..= Doc.toJson doc,
      "signature" Aeson..= maybe Aeson.Null Aeson.String sig
    ]
