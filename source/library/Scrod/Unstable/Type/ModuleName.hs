module Scrod.Unstable.Type.ModuleName where

import qualified Data.Text as Text
import qualified Language.Haskell.Syntax.Module.Name as ModuleName
import qualified Scrod.Unstable.Type.Json as Json

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

toJson :: ModuleName -> Json.Json
toJson (MkModuleName t) = Json.fromText t
