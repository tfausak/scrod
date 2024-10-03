module Scrod.Type.Item where

import qualified Scrod.Type.Name as Name
import qualified Scrod.Type.Position as Position

data Item = Item
  { name :: Name.Name,
    position :: Position.Position
  }
  deriving (Eq, Show)
