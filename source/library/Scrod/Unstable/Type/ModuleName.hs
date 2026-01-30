{-# LANGUAGE DerivingVia #-}

module Scrod.Unstable.Type.ModuleName where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Language.Haskell.Syntax.Module.Name as ModuleName

newtype ModuleName = MkModuleName
  { value :: Text.Text
  }
  deriving (Eq, Ord, Show)
  deriving (Aeson.ToJSON) via Text.Text

fromString :: String -> ModuleName
fromString =
  MkModuleName
    . Text.pack

fromGhc :: ModuleName.ModuleName -> ModuleName
fromGhc =
  fromString
    . ModuleName.moduleNameString
