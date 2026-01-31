module Scrod.Type.Item where

import qualified Scrod.Type.Doc as Doc
import qualified Scrod.Type.ItemKey as ItemKey
import qualified Scrod.Type.ItemName as ItemName

data Item = MkItem
  { key :: ItemKey.ItemKey,
    parentKey :: Maybe ItemKey.ItemKey,
    name :: Maybe ItemName.ItemName,
    documentation :: Doc.Doc
  }
  deriving (Eq, Ord, Show)
