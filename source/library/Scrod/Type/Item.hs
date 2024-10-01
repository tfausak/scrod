module Scrod.Type.Item where

import qualified Scrod.Type.Position as Position

data Item = Item
  { name :: String,
    position :: Position.Position
  }
  deriving (Eq, Show)
