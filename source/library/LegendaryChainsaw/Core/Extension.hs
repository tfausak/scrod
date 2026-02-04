module LegendaryChainsaw.Core.Extension where

import qualified Data.Text as Text

newtype Extension = MkExtension
  { value :: Text.Text
  }
  deriving (Eq, Ord, Show)
