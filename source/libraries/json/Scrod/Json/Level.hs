{-# LANGUAGE OverloadedStrings #-}

module Scrod.Json.Level where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Scrod.Json.Helpers as Helpers
import qualified Scrod.Type.Level as Type

fromJson :: Aeson.Value -> Either String Type.Level
fromJson value = case value of
  Aeson.Object obj -> do
    tagJson <- Helpers.lookupField obj "tag"
    tag <- case tagJson of
      Aeson.String t -> Right t
      _ -> Left "tag must be a string"
    case tag of
      "One" -> Right Type.One
      "Two" -> Right Type.Two
      "Three" -> Right Type.Three
      "Four" -> Right Type.Four
      "Five" -> Right Type.Five
      "Six" -> Right Type.Six
      _ -> Left $ "unknown Level tag: " <> Text.unpack tag
  _ -> Left "Level must be an object"

toJson :: Type.Level -> Aeson.Value
toJson lvl = Aeson.object ["tag" Aeson..= tag]
  where
    tag :: Text.Text
    tag = case lvl of
      Type.One -> "One"
      Type.Two -> "Two"
      Type.Three -> "Three"
      Type.Four -> "Four"
      Type.Five -> "Five"
      Type.Six -> "Six"
