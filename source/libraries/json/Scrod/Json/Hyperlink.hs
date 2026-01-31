{-# LANGUAGE OverloadedStrings #-}

module Scrod.Json.Hyperlink where

import qualified Data.Aeson as Aeson
import qualified Scrod.Json.Helpers as Helpers
import qualified Scrod.Type.Hyperlink as Type

fromJson :: (Aeson.Value -> Either String doc) -> Aeson.Value -> Either String (Type.Hyperlink doc)
fromJson fromJsonDoc value = case value of
  Aeson.Object obj -> do
    urlJson <- Helpers.lookupField obj "url"
    u <- case urlJson of
      Aeson.String t -> Right t
      _ -> Left "url must be a string"
    labelJson <- Helpers.lookupField obj "label"
    lbl <- case labelJson of
      Aeson.Null -> Right Nothing
      _ -> fmap Just (fromJsonDoc labelJson)
    Right $ Type.MkHyperlink {Type.url = u, Type.label = lbl}
  _ -> Left "Hyperlink must be an object"

toJson :: (doc -> Aeson.Value) -> Type.Hyperlink doc -> Aeson.Value
toJson toJsonDoc (Type.MkHyperlink u lbl) =
  Aeson.object
    [ "url" Aeson..= u,
      "label" Aeson..= maybe Aeson.Null toJsonDoc lbl
    ]
