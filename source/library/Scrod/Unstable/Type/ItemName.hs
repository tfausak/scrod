module Scrod.Unstable.Type.ItemName where

import qualified Data.Text as Text
import qualified Scrod.Unstable.Type.Json as Json

newtype ItemName = MkItemName
  { value :: Text.Text
  }
  deriving (Eq, Ord, Show)

toJson :: ItemName -> Json.Json
toJson (MkItemName t) = Json.fromText t
