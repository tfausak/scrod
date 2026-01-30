{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Scrod.Unstable.Type.Example where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Scrod.Unstable.Type.JsonHelpers as JsonHelpers

-- | An example expression with its expected result.
-- Mirrors 'Documentation.Haddock.Types.Example' from haddock-library,
-- but uses 'Text' instead of 'String'.
data Example = MkExample
  { expression :: Text.Text,
    result :: [Text.Text]
  }
  deriving (Eq, Ord, Show)

fromJson :: Aeson.Value -> Either String Example
fromJson = \case
  Aeson.Object obj -> do
    exprJson <- JsonHelpers.lookupField obj "expression"
    expr <- case exprJson of
      Aeson.String t -> Right t
      _ -> Left "expression must be a string"
    resultJson <- JsonHelpers.lookupField obj "result"
    res <- case resultJson of
      Aeson.Array vec -> traverse fromJsonString (Vector.toList vec)
      _ -> Left "result must be an array"
    Right $ MkExample {expression = expr, result = res}
  _ -> Left "Example must be an object"
  where
    fromJsonString :: Aeson.Value -> Either String Text.Text
    fromJsonString = \case
      Aeson.String t -> Right t
      _ -> Left "result elements must be strings"

toJson :: Example -> Aeson.Value
toJson (MkExample expr res) =
  Aeson.object
    [ "expression" Aeson..= expr,
      "result" Aeson..= res
    ]
