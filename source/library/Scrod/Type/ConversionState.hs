module Scrod.Type.ConversionState where

import qualified Numeric.Natural as Natural
import qualified Scrod.Type.ItemKey as ItemKey

newtype ConversionState = MkConversionState
  { nextKey :: Natural.Natural
  }
  deriving (Eq, Ord, Show)

initialState :: ConversionState
initialState = MkConversionState {nextKey = 1}

allocateKey :: ConversionState -> (ItemKey.ItemKey, ConversionState)
allocateKey state =
  let key = ItemKey.MkItemKey {ItemKey.value = nextKey state}
      newState = state {nextKey = nextKey state + 1}
   in (key, newState)
