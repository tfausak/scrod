module Scrod.Type.ModuleName where

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
