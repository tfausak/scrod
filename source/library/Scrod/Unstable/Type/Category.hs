module Scrod.Unstable.Type.Category where

import qualified Data.Text as Text
import qualified GHC.Data.FastString as FastString
import qualified GHC.Unit.Module.Warnings as Warnings
import qualified Scrod.Unstable.Type.Json as Json

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

toJson :: Category -> Json.Json
toJson (MkCategory t) = Json.fromText t
