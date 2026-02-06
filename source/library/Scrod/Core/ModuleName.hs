module Scrod.Core.ModuleName where

import qualified Data.Text as Text

newtype ModuleName = MkModuleName
  { unwrap :: Text.Text
  }
  deriving (Eq, Ord, Show)
