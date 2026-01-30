{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Scrod.Type.TableCell where

import qualified Data.Aeson as Aeson
import qualified Data.Scientific as Scientific
import qualified Numeric.Natural as Natural
import qualified Scrod.Type.JsonHelpers as JsonHelpers

-- | A table cell with colspan, rowspan, and contents.
-- Mirrors 'Documentation.Haddock.Types.TableCell' from haddock-library,
-- but uses 'Natural' instead of 'Int' for colspan and rowspan.
data Cell doc = MkCell
  { colspan :: Natural.Natural,
    rowspan :: Natural.Natural,
    contents :: doc
  }
  deriving (Eq, Ord, Show)

fromJson :: (Aeson.Value -> Either String doc) -> Aeson.Value -> Either String (Cell doc)
fromJson fromJsonDoc = \case
  Aeson.Object obj -> do
    colspanJson <- JsonHelpers.lookupField obj "colspan"
    cs <- fromJsonNatural "colspan" colspanJson
    rowspanJson <- JsonHelpers.lookupField obj "rowspan"
    rs <- fromJsonNatural "rowspan" rowspanJson
    contentsJson <- JsonHelpers.lookupField obj "contents"
    c <- fromJsonDoc contentsJson
    Right $ MkCell {colspan = cs, rowspan = rs, contents = c}
  _ -> Left "Cell must be an object"
  where
    fromJsonNatural :: String -> Aeson.Value -> Either String Natural.Natural
    fromJsonNatural fieldName = \case
      Aeson.Number n -> case Scientific.floatingOrInteger n of
        Right i | i >= 0 -> Right (fromInteger i)
        Right i -> Left $ fieldName <> " must be non-negative, got: " <> show i
        Left (_ :: Double) -> Left $ fieldName <> " must be an integer"
      _ -> Left $ fieldName <> " must be a number"

toJson :: (doc -> Aeson.Value) -> Cell doc -> Aeson.Value
toJson toJsonDoc (MkCell cs rs c) =
  Aeson.object
    [ "colspan" Aeson..= Aeson.Number (Scientific.scientific (fromIntegral cs) 0),
      "rowspan" Aeson..= Aeson.Number (Scientific.scientific (fromIntegral rs) 0),
      "contents" Aeson..= toJsonDoc c
    ]
