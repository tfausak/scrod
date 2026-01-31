{-# LANGUAGE OverloadedStrings #-}

module Scrod.Json.Picture where

import qualified Data.Aeson as Aeson
import qualified Scrod.Json.Helpers as Helpers
import qualified Scrod.Type.Picture as Type

fromJson :: Aeson.Value -> Either String Type.Picture
fromJson value = case value of
  Aeson.Object obj -> do
    uriJson <- Helpers.lookupField obj "uri"
    u <- case uriJson of
      Aeson.String t -> Right t
      _ -> Left "uri must be a string"
    titleJson <- Helpers.lookupField obj "title"
    t <- case titleJson of
      Aeson.Null -> Right Nothing
      Aeson.String txt -> Right (Just txt)
      _ -> Left "title must be a string or null"
    Right $ Type.MkPicture {Type.uri = u, Type.title = t}
  _ -> Left "Picture must be an object"

toJson :: Type.Picture -> Aeson.Value
toJson (Type.MkPicture u t) =
  Aeson.object
    [ "uri" Aeson..= u,
      "title" Aeson..= maybe Aeson.Null Aeson.String t
    ]
