module Scrod.Core.Category where

import qualified Data.Text as Text

newtype Category = MkCategory
  { unwrap :: Text.Text
  }
  deriving (Eq, Ord, Show)
