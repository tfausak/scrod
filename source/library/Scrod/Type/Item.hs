{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Scrod.Type.Item where

import qualified Data.Aeson as Aeson
import qualified Scrod.Type.Doc as Doc
import qualified Scrod.Type.ItemKey as ItemKey
import qualified Scrod.Type.ItemName as ItemName
import qualified Scrod.Type.JsonHelpers as JsonHelpers

data Item = MkItem
  { key :: ItemKey.ItemKey,
    parentKey :: Maybe ItemKey.ItemKey,
    name :: Maybe ItemName.ItemName,
    documentation :: Doc.Doc
  }
  deriving (Eq, Ord, Show)

fromJson :: Aeson.Value -> Either String Item
fromJson = \case
  Aeson.Object obj -> do
    keyJson <- JsonHelpers.lookupField obj "key"
    k <- ItemKey.fromJson keyJson
    parentKeyJson <- JsonHelpers.lookupField obj "parentKey"
    pk <- case parentKeyJson of
      Aeson.Null -> Right Nothing
      _ -> fmap Just (ItemKey.fromJson parentKeyJson)
    nameJson <- JsonHelpers.lookupField obj "name"
    n <- case nameJson of
      Aeson.Null -> Right Nothing
      _ -> fmap Just (ItemName.fromJson nameJson)
    docJson <- JsonHelpers.lookupField obj "documentation"
    doc <- Doc.fromJson docJson
    Right $ MkItem {key = k, parentKey = pk, name = n, documentation = doc}
  _ -> Left "Item must be an object"

toJson :: Item -> Aeson.Value
toJson (MkItem k pk n doc) =
  Aeson.object
    [ "key" Aeson..= ItemKey.toJson k,
      "parentKey" Aeson..= maybe Aeson.Null ItemKey.toJson pk,
      "name" Aeson..= maybe Aeson.Null ItemName.toJson n,
      "documentation" Aeson..= Doc.toJson doc
    ]
