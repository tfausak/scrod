module Scrod.Unstable.Type.Example where

import qualified Data.Text as Text

-- | An example expression with its expected result.
-- Mirrors 'Documentation.Haddock.Types.Example' from haddock-library,
-- but uses 'Text' instead of 'String'.
data Example = MkExample
  { expression :: Text.Text,
    result :: [Text.Text]
  }
  deriving (Eq, Ord, Show)
