{-# LANGUAGE DerivingVia #-}

module Scrod.Unstable.Type.ItemKey where

import qualified Data.Aeson as Aeson
import qualified Numeric.Natural as Natural

newtype ItemKey = MkItemKey
  { value :: Natural.Natural
  }
  deriving (Eq, Ord, Show)
  deriving (Aeson.ToJSON) via Natural.Natural
