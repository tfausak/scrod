{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Scrod.Json.Location where

import qualified Data.Aeson as Aeson
import qualified Scrod.Json.Column as Column
import qualified Scrod.Json.Helpers as Helpers
import qualified Scrod.Json.Line as Line
import qualified Scrod.Type.Location as Type

fromJson :: Aeson.Value -> Either String Type.Location
fromJson = \case
  Aeson.Object obj -> do
    lineJson <- Helpers.lookupField obj "line"
    l <- Line.fromJson lineJson
    columnJson <- Helpers.lookupField obj "column"
    c <- Column.fromJson columnJson
    Right $ Type.MkLocation {Type.line = l, Type.column = c}
  _ -> Left "Location must be an object"

toJson :: Type.Location -> Aeson.Value
toJson (Type.MkLocation l c) =
  Aeson.object
    [ "line" Aeson..= Line.toJson l,
      "column" Aeson..= Column.toJson c
    ]
