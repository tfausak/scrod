module LegendaryChainsaw.Core.Language where

import qualified Data.Text as Text

newtype Language = MkLanguage
  { value :: Text.Text
  }
  deriving (Eq, Ord, Show)
