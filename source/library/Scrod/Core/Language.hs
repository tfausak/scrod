module Scrod.Core.Language where

import qualified Data.Text as Text

newtype Language = MkLanguage
  { unwrap :: Text.Text
  }
  deriving (Eq, Ord, Show)
