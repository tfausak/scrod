{-# LANGUAGE LambdaCase #-}

module Scrod.Type.ItemName where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text

newtype ItemName = MkItemName
  { value :: Text.Text
  }
  deriving (Eq, Ord, Show)

fromJson :: Aeson.Value -> Either String ItemName
fromJson = \case
  Aeson.String txt -> Right (MkItemName txt)
  _ -> Left "ItemName must be a string"

toJson :: ItemName -> Aeson.Value
toJson (MkItemName txt) = Aeson.String txt
