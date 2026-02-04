module LegendaryChainsaw.Core.ItemName where

import qualified Data.Text as Text

newtype ItemName = MkItemName
  { value :: Text.Text
  }
  deriving (Eq, Ord, Show)
