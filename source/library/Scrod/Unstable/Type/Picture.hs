module Scrod.Unstable.Type.Picture where

import qualified Data.Text as Text

-- | A picture/image reference.
-- Mirrors 'Documentation.Haddock.Types.Picture' from haddock-library,
-- but uses 'Text' instead of 'String'.
data Picture = MkPicture
  { uri :: Text.Text,
    title :: Maybe Text.Text
  }
  deriving (Eq, Ord, Show)
