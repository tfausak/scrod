module Scrod.Unstable.Type.Warning where

import qualified Data.Text as Text
import qualified Scrod.Unstable.Type.Category as Category
import qualified Scrod.Unstable.Type.Json as Json

data Warning = MkWarning
  { category :: Category.Category,
    value :: Text.Text
  }
  deriving (Eq, Ord, Show)

toJson :: Warning -> Json.Json
toJson (MkWarning c v) =
  Json.object
    [ (Text.pack "category", Category.toJson c),
      (Text.pack "value", Json.fromText v)
    ]
