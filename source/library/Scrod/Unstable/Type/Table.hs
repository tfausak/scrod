{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Scrod.Unstable.Type.Table where

import qualified Data.Aeson as Aeson
import qualified Data.Vector as Vector
import qualified Scrod.Unstable.Type.JsonHelpers as JsonHelpers
import qualified Scrod.Unstable.Type.TableCell as TableCell

-- | A table with header and body rows.
-- Mirrors 'Documentation.Haddock.Types.Table' from haddock-library.
data Table doc = MkTable
  { headerRows :: [[TableCell.Cell doc]],
    bodyRows :: [[TableCell.Cell doc]]
  }
  deriving (Eq, Ord, Show)

fromJson :: (Aeson.Value -> Either String doc) -> Aeson.Value -> Either String (Table doc)
fromJson fromJsonDoc = \case
  Aeson.Object obj -> do
    headerJson <- JsonHelpers.lookupField obj "headerRows"
    hRows <- parseRows headerJson
    bodyJson <- JsonHelpers.lookupField obj "bodyRows"
    bRows <- parseRows bodyJson
    Right $ MkTable {headerRows = hRows, bodyRows = bRows}
  _ -> Left "Table must be an object"
  where
    parseRows = \case
      Aeson.Array vec -> traverse parseRow (Vector.toList vec)
      _ -> Left "rows must be an array"
    parseRow = \case
      Aeson.Array vec -> traverse (TableCell.fromJson fromJsonDoc) (Vector.toList vec)
      _ -> Left "row must be an array"

toJson :: (doc -> Aeson.Value) -> Table doc -> Aeson.Value
toJson toJsonDoc (MkTable hRows bRows) =
  Aeson.object
    [ "headerRows" Aeson..= fmap (fmap (TableCell.toJson toJsonDoc)) hRows,
      "bodyRows" Aeson..= fmap (fmap (TableCell.toJson toJsonDoc)) bRows
    ]
