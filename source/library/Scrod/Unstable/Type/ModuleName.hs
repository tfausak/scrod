{-# LANGUAGE LambdaCase #-}

module Scrod.Unstable.Type.ModuleName where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Language.Haskell.Syntax.Module.Name as ModuleName

newtype ModuleName = MkModuleName
  { value :: Text.Text
  }
  deriving (Eq, Ord, Show)

fromString :: String -> ModuleName
fromString =
  MkModuleName
    . Text.pack

fromGhc :: ModuleName.ModuleName -> ModuleName
fromGhc =
  fromString
    . ModuleName.moduleNameString

fromJson :: Aeson.Value -> Either String ModuleName
fromJson = \case
  Aeson.String txt -> Right (MkModuleName txt)
  _ -> Left "ModuleName must be a string"

toJson :: ModuleName -> Aeson.Value
toJson (MkModuleName txt) = Aeson.String txt
