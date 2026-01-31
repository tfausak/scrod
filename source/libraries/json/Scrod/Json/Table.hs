{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Scrod.Json.Table where

import qualified Data.Aeson as Aeson
import qualified Data.Vector as Vector
import qualified Scrod.Json.Helpers as Helpers
import qualified Scrod.Json.TableCell as TableCell
import qualified Scrod.Type.Table as Type

fromJson :: (Aeson.Value -> Either String doc) -> Aeson.Value -> Either String (Type.Table doc)
fromJson fromJsonDoc = \case
  Aeson.Object obj -> do
    headerJson <- Helpers.lookupField obj "headerRows"
    hRows <- parseRows headerJson
    bodyJson <- Helpers.lookupField obj "bodyRows"
    bRows <- parseRows bodyJson
    Right $ Type.MkTable {Type.headerRows = hRows, Type.bodyRows = bRows}
  _ -> Left "Table must be an object"
  where
    parseRows = \case
      Aeson.Array vec -> traverse parseRow (Vector.toList vec)
      _ -> Left "rows must be an array"
    parseRow = \case
      Aeson.Array vec -> traverse (TableCell.fromJson fromJsonDoc) (Vector.toList vec)
      _ -> Left "row must be an array"

toJson :: (doc -> Aeson.Value) -> Type.Table doc -> Aeson.Value
toJson toJsonDoc (Type.MkTable hRows bRows) =
  Aeson.object
    [ "headerRows" Aeson..= fmap (fmap (TableCell.toJson toJsonDoc)) hRows,
      "bodyRows" Aeson..= fmap (fmap (TableCell.toJson toJsonDoc)) bRows
    ]
