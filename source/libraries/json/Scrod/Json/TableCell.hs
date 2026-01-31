{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Scrod.Json.TableCell where

import qualified Data.Aeson as Aeson
import qualified Data.Scientific as Scientific
import qualified Numeric.Natural as Natural
import qualified Scrod.Json.Helpers as Helpers
import qualified Scrod.Type.TableCell as Type

fromJson :: (Aeson.Value -> Either String doc) -> Aeson.Value -> Either String (Type.Cell doc)
fromJson fromJsonDoc = \case
  Aeson.Object obj -> do
    colspanJson <- Helpers.lookupField obj "colspan"
    cs <- fromJsonNatural "colspan" colspanJson
    rowspanJson <- Helpers.lookupField obj "rowspan"
    rs <- fromJsonNatural "rowspan" rowspanJson
    contentsJson <- Helpers.lookupField obj "contents"
    c <- fromJsonDoc contentsJson
    Right $ Type.MkCell {Type.colspan = cs, Type.rowspan = rs, Type.contents = c}
  _ -> Left "Cell must be an object"
  where
    fromJsonNatural :: String -> Aeson.Value -> Either String Natural.Natural
    fromJsonNatural fieldName = \case
      Aeson.Number n -> case Scientific.floatingOrInteger n of
        Right i | i >= 0 -> Right (fromInteger i)
        Right i -> Left $ fieldName <> " must be non-negative, got: " <> show i
        Left (_ :: Double) -> Left $ fieldName <> " must be an integer"
      _ -> Left $ fieldName <> " must be a number"

toJson :: (doc -> Aeson.Value) -> Type.Cell doc -> Aeson.Value
toJson toJsonDoc (Type.MkCell cs rs c) =
  Aeson.object
    [ "colspan" Aeson..= Aeson.Number (Scientific.scientific (fromIntegral cs) 0),
      "rowspan" Aeson..= Aeson.Number (Scientific.scientific (fromIntegral rs) 0),
      "contents" Aeson..= toJsonDoc c
    ]
