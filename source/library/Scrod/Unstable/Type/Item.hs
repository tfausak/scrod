module Scrod.Unstable.Type.Item where

import qualified Data.Text as Text
import qualified Scrod.Unstable.Type.Doc as Doc
import qualified Scrod.Unstable.Type.ItemKey as ItemKey
import qualified Scrod.Unstable.Type.ItemName as ItemName
import qualified Scrod.Unstable.Type.Json as Json

data Item = MkItem
  { key :: ItemKey.ItemKey,
    parentKey :: Maybe ItemKey.ItemKey,
    name :: Maybe ItemName.ItemName,
    documentation :: Doc.Doc
  }
  deriving (Eq, Ord, Show)

toJson :: Item -> Json.Json
toJson (MkItem k pk n d) =
  Json.object
    [ (Text.pack "key", ItemKey.toJson k),
      (Text.pack "parentKey", maybe Json.Null ItemKey.toJson pk),
      (Text.pack "name", maybe Json.Null ItemName.toJson n),
      (Text.pack "documentation", Doc.toJson d)
    ]
