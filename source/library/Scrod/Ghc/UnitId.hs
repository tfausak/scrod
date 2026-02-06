module Scrod.Ghc.UnitId where

import qualified GHC.Data.FastString as FastString
import qualified GHC.Unit.Types as Unit

empty :: Unit.UnitId
empty = Unit.UnitId {Unit.unitIdFS = FastString.nilFS}
