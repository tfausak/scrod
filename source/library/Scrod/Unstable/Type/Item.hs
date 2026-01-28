module Scrod.Unstable.Type.Item where

import qualified Scrod.Unstable.Type.Doc as Doc
import qualified Scrod.Unstable.Type.ItemKey as ItemKey
import qualified Scrod.Unstable.Type.ItemName as ItemName

data Item = MkItem
  { key :: ItemKey.ItemKey,
    parentKey :: Maybe ItemKey.ItemKey,
    name :: Maybe ItemName.ItemName,
    documentation :: Doc.Doc
  }
  deriving (Eq, Ord, Show)
