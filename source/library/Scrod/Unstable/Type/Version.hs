module Scrod.Unstable.Type.Version where

newtype Version = MkVersion
  { value :: [Int]
  }
  deriving (Eq, Ord, Show)

fromHaddock :: [Int] -> Version
fromHaddock = MkVersion
