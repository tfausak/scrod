-- | Type class for converting values into JSON.
module Scrod.Json.ToJson where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified Numeric.Natural as Natural
import qualified Scrod.Json.Value as Json

-- | Convert a value to a JSON 'Json.Value'.
class ToJson a where
  toJson :: a -> Json.Value

instance ToJson Bool where
  toJson = Json.boolean

instance ToJson Text.Text where
  toJson = Json.text

instance ToJson Natural.Natural where
  toJson = Json.integral

instance (ToJson a) => ToJson (Maybe a) where
  toJson = maybe Json.null toJson

instance (ToJson a) => ToJson [a] where
  toJson = Json.arrayOf toJson

instance (ToJson a) => ToJson (NonEmpty.NonEmpty a) where
  toJson = toJson . NonEmpty.toList
