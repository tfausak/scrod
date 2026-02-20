{-# LANGUAGE TemplateHaskellQuotes #-}

module Scrod.Core.Level where

import qualified Scrod.Json.ToJson as ToJson
import qualified Scrod.Json.Value as Json
import qualified Scrod.Schema as Schema
import qualified Scrod.Spec as Spec

-- | A header level from 1 to 6 inclusive.
data Level
  = One
  | Two
  | Three
  | Four
  | Five
  | Six
  deriving (Eq, Ord, Show)

-- | Convert an integer to a 'Level', clamping to the valid range.
fromInt :: Int -> Level
fromInt n
  | n <= 1 = One
  | n == 2 = Two
  | n == 3 = Three
  | n == 4 = Four
  | n == 5 = Five
  | otherwise = Six

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

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = do
  Spec.named s 'fromInt $ do
    Spec.it s "maps 1 to One" $ do
      Spec.assertEq s (fromInt 1) One

    Spec.it s "maps 2 to Two" $ do
      Spec.assertEq s (fromInt 2) Two

    Spec.it s "maps 3 to Three" $ do
      Spec.assertEq s (fromInt 3) Three

    Spec.it s "maps 4 to Four" $ do
      Spec.assertEq s (fromInt 4) Four

    Spec.it s "maps 5 to Five" $ do
      Spec.assertEq s (fromInt 5) Five

    Spec.it s "maps 6 to Six" $ do
      Spec.assertEq s (fromInt 6) Six

    Spec.it s "clamps zero to One" $ do
      Spec.assertEq s (fromInt 0) One

    Spec.it s "clamps negative to One" $ do
      Spec.assertEq s (fromInt (-1)) One

    Spec.it s "clamps 7 to Six" $ do
      Spec.assertEq s (fromInt 7) Six

    Spec.it s "clamps large value to Six" $ do
      Spec.assertEq s (fromInt 100) Six
