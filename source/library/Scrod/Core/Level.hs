module Scrod.Core.Level where

import qualified Scrod.Json.ToJson as ToJson
import qualified Scrod.Json.Value as Json
import qualified Scrod.Schema as Schema

-- | A header level from 1 to 6 inclusive.
data Level
  = One
  | Two
  | Three
  | Four
  | Five
  | Six
  deriving (Eq, Ord, Show)

instance ToJson.ToJson Level where
  toJson l = Json.integer $ case l of
    One -> 1
    Two -> 2
    Three -> 3
    Four -> 4
    Five -> 5
    Six -> 6

instance Schema.ToSchema Level where
  toSchema _ =
    pure . Schema.MkSchema $
      Json.object
        [ ("type", Json.string "integer"),
          ("minimum", Json.integer 1),
          ("maximum", Json.integer 6)
        ]
