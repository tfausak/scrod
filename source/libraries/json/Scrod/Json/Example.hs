{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Scrod.Json.Example where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Scrod.Json.Helpers as Helpers
import qualified Scrod.Type.Example as Type

fromJson :: Aeson.Value -> Either String Type.Example
fromJson = \case
  Aeson.Object obj -> do
    exprJson <- Helpers.lookupField obj "expression"
    expr <- case exprJson of
      Aeson.String t -> Right t
      _ -> Left "expression must be a string"
    resultJson <- Helpers.lookupField obj "result"
    res <- case resultJson of
      Aeson.Array vec -> traverse fromJsonString (Vector.toList vec)
      _ -> Left "result must be an array"
    Right $ Type.MkExample {Type.expression = expr, Type.result = res}
  _ -> Left "Example must be an object"
  where
    fromJsonString :: Aeson.Value -> Either String Text.Text
    fromJsonString = \case
      Aeson.String t -> Right t
      _ -> Left "result elements must be strings"

toJson :: Type.Example -> Aeson.Value
toJson (Type.MkExample expr res) =
  Aeson.object
    [ "expression" Aeson..= expr,
      "result" Aeson..= res
    ]
