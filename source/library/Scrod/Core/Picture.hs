module Scrod.Core.Picture where

import qualified Data.Text as Text

-- | A picture/image reference.
data Picture = MkPicture
  { uri :: Text.Text,
    title :: Maybe Text.Text
  }
  deriving (Eq, Ord, Show)
