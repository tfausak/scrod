module Scrod.Type.Warning where

import qualified Data.Text as Text
import qualified Scrod.Type.Category as Category

data Warning = MkWarning
  { category :: Category.Category,
    value :: Text.Text
  }
  deriving (Eq, Ord, Show)
