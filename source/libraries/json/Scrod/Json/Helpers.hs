module Scrod.Json.Helpers where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Text as Text
import qualified Numeric.Natural as Natural

lookupField :: KeyMap.KeyMap Aeson.Value -> Text.Text -> Either String Aeson.Value
lookupField obj k = case KeyMap.lookup (Key.fromText k) obj of
  Just v -> Right v
  Nothing -> Left $ "missing field: " <> Text.unpack k

fromJsonNatural :: Aeson.Value -> Either String Natural.Natural
fromJsonNatural value = case Aeson.fromJSON value of
  Aeson.Success n -> Right n
  Aeson.Error msg -> Left msg

fromJsonInt :: Aeson.Value -> Either String Int
fromJsonInt value = case Aeson.fromJSON value of
  Aeson.Success n -> Right n
  Aeson.Error msg -> Left msg
