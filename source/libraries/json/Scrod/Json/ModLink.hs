{-# LANGUAGE OverloadedStrings #-}

module Scrod.Json.ModLink where

import qualified Data.Aeson as Aeson
import qualified Scrod.Json.Helpers as Helpers
import qualified Scrod.Json.ModuleName as ModuleName
import qualified Scrod.Type.ModLink as Type

fromJson :: (Aeson.Value -> Either String doc) -> Aeson.Value -> Either String (Type.ModLink doc)
fromJson fromJsonDoc value = case value of
  Aeson.Object obj -> do
    nameJson <- Helpers.lookupField obj "name"
    n <- ModuleName.fromJson nameJson
    labelJson <- Helpers.lookupField obj "label"
    lbl <- case labelJson of
      Aeson.Null -> Right Nothing
      _ -> fmap Just (fromJsonDoc labelJson)
    Right $ Type.MkModLink {Type.name = n, Type.label = lbl}
  _ -> Left "ModLink must be an object"

toJson :: (doc -> Aeson.Value) -> Type.ModLink doc -> Aeson.Value
toJson toJsonDoc (Type.MkModLink n lbl) =
  Aeson.object
    [ "name" Aeson..= ModuleName.toJson n,
      "label" Aeson..= maybe Aeson.Null toJsonDoc lbl
    ]
