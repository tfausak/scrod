module LegendaryChainsaw.Core.PackageName where

import qualified Data.Text as Text

newtype PackageName = MkPackageName
  { value :: Text.Text
  }
  deriving (Eq, Ord, Show)
