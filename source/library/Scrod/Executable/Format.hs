{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Output format selection (@html@ or @json@).
module Scrod.Executable.Format where

import qualified Control.Monad.Catch as Exception
import qualified GHC.Stack as Stack
import qualified Scrod.Spec as Spec

data Format
  = Html
  | Json
  deriving (Eq, Ord, Show)

fromString :: (Stack.HasCallStack, Exception.MonadThrow m) => String -> m Format
fromString string = case string of
  "html" -> pure Html
  "json" -> pure Json
  _ -> Exception.throwM . userError $ "invalid format: " <> show string

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = do
  Spec.named s 'fromString $ do
    Spec.it s "parses html" $ do
      Spec.assertEq s (fromString "html") $ Just Html

    Spec.it s "parses json" $ do
      Spec.assertEq s (fromString "json") $ Just Json

    Spec.it s "fails with invalid input" $ do
      Spec.assertEq s (fromString "invalid") Nothing
