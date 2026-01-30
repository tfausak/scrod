{-# LANGUAGE LambdaCase #-}

module Scrod.Unstable.Type.PackageName where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text

newtype PackageName = MkPackageName
  { value :: Text.Text
  }
  deriving (Eq, Ord, Show)

fromString :: String -> PackageName
fromString =
  MkPackageName
    . Text.pack

fromJson :: Aeson.Value -> Either String PackageName
fromJson = \case
  Aeson.String txt -> Right (MkPackageName txt)
  _ -> Left "PackageName must be a string"

toJson :: PackageName -> Aeson.Value
toJson (MkPackageName txt) = Aeson.String txt
