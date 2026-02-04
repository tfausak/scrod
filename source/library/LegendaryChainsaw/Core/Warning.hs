module LegendaryChainsaw.Core.Warning where

import qualified Data.Text as Text
import qualified LegendaryChainsaw.Core.Category as Category

data Warning = MkWarning
  { category :: Category.Category,
    value :: Text.Text
  }
  deriving (Eq, Ord, Show)
