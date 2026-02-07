{-# LANGUAGE TemplateHaskellQuotes #-}

module Scrod.Executable.Style where

import qualified Control.Monad.Catch as Exception
import qualified GHC.Stack as Stack
import qualified Scrod.Spec as Spec

data Style
  = Bird
  | Latex
  | None
  deriving (Eq, Ord, Show)

fromString :: (Stack.HasCallStack, Exception.MonadThrow m) => String -> m Style
fromString string = case string of
  "bird" -> pure Bird
  "latex" -> pure Latex
  "none" -> pure None
  _ -> Exception.throwM . userError $ "invalid style: " <> show string

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = do
  Spec.named s 'fromString $ do
    Spec.it s "parses bird" $ do
      Spec.assertEq s (fromString "bird") $ Just Bird

    Spec.it s "parses latex" $ do
      Spec.assertEq s (fromString "latex") $ Just Latex

    Spec.it s "parses none" $ do
      Spec.assertEq s (fromString "none") $ Just None

    Spec.it s "fails with invalid input" $ do
      Spec.assertEq s (fromString "invalid") Nothing
