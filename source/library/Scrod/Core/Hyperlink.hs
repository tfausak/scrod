module Scrod.Core.Hyperlink where

import qualified Data.Text as Text

-- | A hyperlink with an optional label.
data Hyperlink doc = MkHyperlink
  { url :: Text.Text,
    label :: Maybe doc
  }
  deriving (Eq, Ord, Show)
