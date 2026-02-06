module Scrod.Core.Extension where

import qualified Data.Text as Text

newtype Extension = MkExtension
  { unwrap :: Text.Text
  }
  deriving (Eq, Ord, Show)
