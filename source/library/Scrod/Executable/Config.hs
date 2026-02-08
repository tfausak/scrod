{-# LANGUAGE TemplateHaskellQuotes #-}

module Scrod.Executable.Config where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified GHC.Stack as Stack
import qualified Scrod.Executable.Flag as Flag
import qualified Scrod.Executable.Format as Format
import qualified Scrod.Extra.Read as Read
import qualified Scrod.Spec as Spec

data Config = MkConfig
  { format :: Format.Format,
    help :: Bool,
    unlit :: Bool,
    version :: Bool
  }
  deriving (Eq, Ord, Show)

fromFlags :: (Stack.HasCallStack, Exception.MonadThrow m) => [Flag.Flag] -> m Config
fromFlags = Monad.foldM applyFlag initial

applyFlag :: (Stack.HasCallStack, Exception.MonadThrow m) => Config -> Flag.Flag -> m Config
applyFlag config flag = case flag of
  Flag.Format string -> do
    fmt <- Format.fromString string
    pure config {format = fmt}
  Flag.Help maybeString -> case maybeString of
    Nothing -> pure config {help = True}
    Just string -> do
      bool <- Read.readM string
      pure config {help = bool}
  Flag.Unlit maybeString -> case maybeString of
    Nothing -> pure config {unlit = True}
    Just string -> do
      bool <- Read.readM string
      pure config {unlit = bool}
  Flag.Version maybeString -> case maybeString of
    Nothing -> pure config {version = True}
    Just string -> do
      bool <- Read.readM string
      pure config {version = bool}

initial :: Config
initial =
  MkConfig
    { format = Format.Json,
      help = False,
      unlit = False,
      version = False
    }

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = do
  Spec.named s 'fromFlags $ do
    Spec.it s "works with no flags" $ do
      Spec.assertEq s (fromFlags []) $ Just initial

    Spec.describe s "format" $ do
      Spec.it s "defaults to json" $ do
        Spec.assertEq s (fromFlags []) $ Just initial

      Spec.it s "works with json" $ do
        Spec.assertEq s (fromFlags [Flag.Format "json"]) $ Just initial

      Spec.it s "works with html" $ do
        Spec.assertEq s (fromFlags [Flag.Format "html"]) $ Just initial {format = Format.Html}

      Spec.it s "fails with invalid format" $ do
        Spec.assertEq s (fromFlags [Flag.Format "invalid"]) Nothing

    Spec.describe s "unlit" $ do
      Spec.it s "works with nothing" $ do
        Spec.assertEq s (fromFlags [Flag.Unlit Nothing]) $ Just initial {unlit = True}

      Spec.it s "works with just false" $ do
        Spec.assertEq s (fromFlags [Flag.Unlit $ Just "False"]) $ Just initial

      Spec.it s "works with just true" $ do
        Spec.assertEq s (fromFlags [Flag.Unlit $ Just "True"]) $ Just initial {unlit = True}

      Spec.it s "fails with just invalid" $ do
        Spec.assertEq s (fromFlags [Flag.Unlit $ Just "invalid"]) Nothing

    Spec.describe s "help" $ do
      Spec.it s "works with nothing" $ do
        Spec.assertEq s (fromFlags [Flag.Help Nothing]) $ Just initial {help = True}

      Spec.it s "works with just false" $ do
        Spec.assertEq s (fromFlags [Flag.Help $ Just "False"]) $ Just initial

      Spec.it s "works with just true" $ do
        Spec.assertEq s (fromFlags [Flag.Help $ Just "True"]) $ Just initial {help = True}

      Spec.it s "picks the last flag" $ do
        Spec.assertEq s (fromFlags [Flag.Help $ Just "False", Flag.Help Nothing]) $ Just initial {help = True}

      Spec.it s "fails with just invalid" $ do
        Spec.assertEq s (fromFlags [Flag.Help $ Just "invalid"]) Nothing
