{-# LANGUAGE OverloadedStrings #-}

module Scrod.Json.TableCell where

import qualified Data.Aeson as Aeson
import qualified Scrod.Json.Helpers as Helpers
import qualified Scrod.Type.TableCell as Type

fromJson :: (Aeson.Value -> Either String doc) -> Aeson.Value -> Either String (Type.Cell doc)
fromJson fromJsonDoc value = case value of
  Aeson.Object obj -> do
    colspanJson <- Helpers.lookupField obj "colspan"
    cs <- Helpers.fromJsonNatural colspanJson
    rowspanJson <- Helpers.lookupField obj "rowspan"
    rs <- Helpers.fromJsonNatural rowspanJson
    contentsJson <- Helpers.lookupField obj "contents"
    c <- fromJsonDoc contentsJson
    Right $ Type.MkCell {Type.colspan = cs, Type.rowspan = rs, Type.contents = c}
  _ -> Left "Cell must be an object"

toJson :: (doc -> Aeson.Value) -> Type.Cell doc -> Aeson.Value
toJson toJsonDoc (Type.MkCell cs rs c) =
  Aeson.object
    [ "colspan" Aeson..= Aeson.toJSON cs,
      "rowspan" Aeson..= Aeson.toJSON rs,
      "contents" Aeson..= toJsonDoc c
    ]
