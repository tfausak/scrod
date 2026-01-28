module Scrod.Unstable.Type.Level where

import qualified Data.Text as Text
import qualified Scrod.Unstable.Type.Json as Json

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

toJson :: Level -> Json.Json
toJson level = case level of
  One -> Json.tag (Text.pack "One")
  Two -> Json.tag (Text.pack "Two")
  Three -> Json.tag (Text.pack "Three")
  Four -> Json.tag (Text.pack "Four")
  Five -> Json.tag (Text.pack "Five")
  Six -> Json.tag (Text.pack "Six")
