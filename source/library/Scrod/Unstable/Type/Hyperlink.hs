module Scrod.Unstable.Type.Hyperlink where

import qualified Data.Text as Text

-- | A hyperlink with an optional label.
-- Mirrors 'Documentation.Haddock.Types.Hyperlink' from haddock-library,
-- but uses 'Text' instead of 'String' for the URL.
data Hyperlink doc = MkHyperlink
  { url :: Text.Text,
    label :: Maybe doc
  }
  deriving (Eq, Ord, Show)
