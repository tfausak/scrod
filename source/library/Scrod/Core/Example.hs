module Scrod.Core.Example where

import qualified Data.Text as Text

-- | An example expression with its expected result.
data Example = MkExample
  { expression :: Text.Text,
    result :: [Text.Text]
  }
  deriving (Eq, Ord, Show)
