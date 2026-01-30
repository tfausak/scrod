{-# LANGUAGE LambdaCase #-}

module Scrod.Unstable.Type.Category where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified GHC.Data.FastString as FastString
import qualified GHC.Unit.Module.Warnings as Warnings

newtype Category = MkCategory
  { value :: Text.Text
  }
  deriving (Eq, Ord, Show)

fromString :: String -> Category
fromString =
  MkCategory
    . Text.pack

fromGhc :: Warnings.WarningCategory -> Category
fromGhc (Warnings.WarningCategory fastString) =
  fromString $ FastString.unpackFS fastString

fromJson :: Aeson.Value -> Either String Category
fromJson = \case
  Aeson.String txt -> Right (MkCategory txt)
  _ -> Left "Category must be a string"

toJson :: Category -> Aeson.Value
toJson (MkCategory txt) = Aeson.String txt
