module LegendaryChainsaw.Core.Level where

-- | A header level from 1 to 6 inclusive.
data Level
  = One
  | Two
  | Three
  | Four
  | Five
  | Six
  deriving (Eq, Ord, Show)
