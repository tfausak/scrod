module Scrod.Json.Helpers where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Text as Text

lookupField :: KeyMap.KeyMap Aeson.Value -> Text.Text -> Either String Aeson.Value
lookupField obj k = case KeyMap.lookup (Key.fromText k) obj of
  Just v -> Right v
  Nothing -> Left $ "missing field: " <> Text.unpack k
