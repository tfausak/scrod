{-# LANGUAGE TemplateHaskellQuotes #-}

module LegendaryChainsaw.Extra.Either where

import qualified Data.Void as Void
import qualified LegendaryChainsaw.Spec as Spec

hush :: Either x a -> Maybe a
hush = either (const Nothing) Just

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = do
  Spec.named s 'hush $ do
    Spec.it s "works with left" $ do
      Spec.assertEq s (hush (Left () :: Either () Void.Void)) Nothing

    Spec.it s "works with right" $ do
      Spec.assertEq s (hush (Right () :: Either Void.Void ())) $ Just ()
