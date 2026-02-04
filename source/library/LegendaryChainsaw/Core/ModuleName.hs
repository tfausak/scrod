module LegendaryChainsaw.Core.ModuleName where

import qualified Data.Text as Text

newtype ModuleName = MkModuleName
  { value :: Text.Text
  }
  deriving (Eq, Ord, Show)
