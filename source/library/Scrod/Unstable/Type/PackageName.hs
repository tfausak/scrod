module Scrod.Unstable.Type.PackageName where

import qualified Data.Text as Text
import qualified Scrod.Unstable.Type.Json as Json

newtype PackageName = MkPackageName
  { value :: Text.Text
  }
  deriving (Eq, Ord, Show)

fromString :: String -> PackageName
fromString =
  MkPackageName
    . Text.pack

toJson :: PackageName -> Json.Json
toJson (MkPackageName t) = Json.fromText t
