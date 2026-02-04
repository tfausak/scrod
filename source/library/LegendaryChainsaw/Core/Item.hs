module LegendaryChainsaw.Core.Item where

import qualified Data.Text as Text
import qualified LegendaryChainsaw.Core.Doc as Doc
import qualified LegendaryChainsaw.Core.ItemKey as ItemKey
import qualified LegendaryChainsaw.Core.ItemKind as ItemKind
import qualified LegendaryChainsaw.Core.ItemName as ItemName

data Item = MkItem
  { key :: ItemKey.ItemKey,
    kind :: ItemKind.ItemKind,
    parentKey :: Maybe ItemKey.ItemKey,
    name :: Maybe ItemName.ItemName,
    documentation :: Doc.Doc,
    signature :: Maybe Text.Text
  }
  deriving (Eq, Ord, Show)
