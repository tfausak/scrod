module Scrod.Core.ItemName where

import qualified Data.Text as Text

newtype ItemName = MkItemName
  { unwrap :: Text.Text
  }
  deriving (Eq, Ord, Show)
