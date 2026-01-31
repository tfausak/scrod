module Scrod.Type.Level where

-- | A header level from 1 to 6 inclusive.
-- This type ensures only valid HTML header levels can be represented.
data Level
  = One
  | Two
  | Three
  | Four
  | Five
  | Six
  deriving (Eq, Ord, Show)

-- | Try to convert an 'Integral' value to a 'Level'.
-- Returns 'Nothing' if the value is not in the range 1-6.
fromIntegral :: (Integral a) => a -> Maybe Level
fromIntegral n = case n of
  1 -> Just One
  2 -> Just Two
  3 -> Just Three
  4 -> Just Four
  5 -> Just Five
  6 -> Just Six
  _ -> Nothing
