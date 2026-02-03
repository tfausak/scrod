{-# LANGUAGE TemplateHaskellQuotes #-}

module LegendaryChainsaw.Extra.Semigroup where

import qualified LegendaryChainsaw.Spec as Spec

around :: Semigroup a => a -> a -> a -> a
around before after = (before <>) . (<> after)

spec :: (Applicative m) => Spec.Spec m n -> n ()
spec s = do
  Spec.named s 'around $ do
    Spec.it s "works" $ do
      Spec.assertEq s (around "<" ">" "html") "<html>"
