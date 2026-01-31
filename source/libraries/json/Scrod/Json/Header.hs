{-# LANGUAGE OverloadedStrings #-}

module Scrod.Json.Header where

import qualified Data.Aeson as Aeson
import qualified Scrod.Json.Helpers as Helpers
import qualified Scrod.Json.Level as Level
import qualified Scrod.Type.Header as Type

fromJson :: (Aeson.Value -> Either String doc) -> Aeson.Value -> Either String (Type.Header doc)
fromJson fromJsonDoc value = case value of
  Aeson.Object obj -> do
    lvlJson <- Helpers.lookupField obj "level"
    lvl <- Level.fromJson lvlJson
    titleJson <- Helpers.lookupField obj "title"
    t <- fromJsonDoc titleJson
    Right $ Type.MkHeader {Type.level = lvl, Type.title = t}
  _ -> Left "Header must be an object"

toJson :: (doc -> Aeson.Value) -> Type.Header doc -> Aeson.Value
toJson toJsonDoc (Type.MkHeader lvl t) =
  Aeson.object
    [ "level" Aeson..= Level.toJson lvl,
      "title" Aeson..= toJsonDoc t
    ]
