module Scrod.Core.Level where

import Scrod.Json.ToJson (ToJson (toJson))
import qualified Scrod.Json.Value as Json
import Scrod.Schema (Schema (MkSchema), ToSchema (toSchema))

-- | A header level from 1 to 6 inclusive.
data Level
  = One
  | Two
  | Three
  | Four
  | Five
  | Six
  deriving (Eq, Ord, Show)

instance ToJson Level where
  toJson l = Json.integer $ case l of
    One -> 1
    Two -> 2
    Three -> 3
    Four -> 4
    Five -> 5
    Six -> 6

instance ToSchema Level where
  toSchema _ =
    pure . MkSchema $
      Json.object
        [ ("type", Json.string "integer"),
          ("minimum", Json.integer 1),
          ("maximum", Json.integer 6)
        ]
