{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Scrod.Unstable.Type.Level where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Scrod.Unstable.Type.JsonHelpers as JsonHelpers

-- | A header level from 1 to 6 inclusive.
-- This type ensures only valid HTML header levels can be represented.
data Level
  = One
  | Two
  | Three
  | Four
  | Five
  | Six
  deriving (Eq, Ord, Show)

fromJson :: Aeson.Value -> Either String Level
fromJson = \case
  Aeson.Object obj -> do
    tagJson <- JsonHelpers.lookupField obj "tag"
    tag <- case tagJson of
      Aeson.String t -> Right t
      _ -> Left "tag must be a string"
    case tag of
      "One" -> Right One
      "Two" -> Right Two
      "Three" -> Right Three
      "Four" -> Right Four
      "Five" -> Right Five
      "Six" -> Right Six
      _ -> Left $ "unknown Level tag: " <> Text.unpack tag
  _ -> Left "Level must be an object"

toJson :: Level -> Aeson.Value
toJson lvl = Aeson.object ["tag" Aeson..= tag]
  where
    tag :: Text.Text
    tag = case lvl of
      One -> "One"
      Two -> "Two"
      Three -> "Three"
      Four -> "Four"
      Five -> "Five"
      Six -> "Six"

-- | Try to convert an 'Integral' value to a 'Level'.
-- Returns 'Nothing' if the value is not in the range 1-6.
fromIntegral :: (Integral a) => a -> Maybe Level
fromIntegral n = case n of
  1 -> Just One
  2 -> Just Two
  3 -> Just Three
  4 -> Just Four
  5 -> Just Five
  6 -> Just Six
  _ -> Nothing
