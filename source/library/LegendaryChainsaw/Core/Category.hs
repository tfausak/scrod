module LegendaryChainsaw.Core.Category where

import qualified Data.Text as Text

newtype Category = MkCategory
  { value :: Text.Text
  }
  deriving (Eq, Ord, Show)
