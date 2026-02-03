{-# LANGUAGE TemplateHaskellQuotes #-}

module LegendaryChainsaw.Extra.Either where

import qualified Control.Monad.Catch as Exception
import qualified Data.Void as Void
import qualified GHC.Stack as Stack
import qualified LegendaryChainsaw.Spec as Spec

hush :: Either x a -> Maybe a
hush = either (const Nothing) Just

throw :: (Stack.HasCallStack, Exception.Exception e, Exception.MonadThrow m) => Either e a -> m a
throw = either Exception.throwM pure

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = do
  Spec.named s 'hush $ do
    Spec.it s "works with left" $ do
      Spec.assertEq s (hush (Left () :: Either () Void.Void)) Nothing

    Spec.it s "works with right" $ do
      Spec.assertEq s (hush (Right () :: Either Void.Void ())) $ Just ()

  Spec.named s 'throw $ do
    Spec.it s "works with left" $ do
      Spec.assertEq s (throw (Left (userError "") :: Either IOError Void.Void)) Nothing

    Spec.it s "works with right" $ do
      Spec.assertEq s (throw (Right () :: Either Void.Void ())) $ Just ()
