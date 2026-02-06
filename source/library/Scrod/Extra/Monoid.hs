{-# LANGUAGE TemplateHaskellQuotes #-}

module Scrod.Extra.Monoid where

import qualified Scrod.Spec as Spec

sepBy :: (Monoid a) => a -> [a] -> a
sepBy s xs = case xs of
  [] -> mempty
  h : t -> h <> foldMap (s <>) t

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = do
  Spec.named s 'sepBy $ do
    Spec.it s "works with an empty list" $ do
      Spec.assertEq s (sepBy "," []) ""

    Spec.it s "works with one element" $ do
      Spec.assertEq s (sepBy "," ["a"]) "a"

    Spec.it s "works with two elements" $ do
      Spec.assertEq s (sepBy "," ["a", "b"]) "a,b"
