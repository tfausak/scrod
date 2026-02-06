{-# LANGUAGE TemplateHaskellQuotes #-}

module Scrod.Extra.Maybe where

import qualified Data.Void as Void
import qualified Scrod.Spec as Spec

note :: e -> Maybe a -> Either e a
note x = maybe (Left x) Right

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = do
  Spec.named s 'note $ do
    Spec.it s "works with nothing" $ do
      Spec.assertEq s (note () Nothing) (Left () :: Either () Void.Void)

    Spec.it s "works with just" $ do
      Spec.assertEq s (note False $ Just ()) $ Right ()
