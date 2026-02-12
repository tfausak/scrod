{-# LANGUAGE TemplateHaskellQuotes #-}

-- | CLI configuration record, built from parsed 'Flag.Flag' values.
module Scrod.Executable.Config where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified GHC.Stack as Stack
import qualified Scrod.Executable.Flag as Flag
import qualified Scrod.Extra.Read as Read
import qualified Scrod.Spec as Spec

data Config = MkConfig
  { help :: Bool,
    literate :: Bool,
    schema :: Bool,
    signature :: Bool,
    version :: Bool
  }
  deriving (Eq, Ord, Show)

fromFlags :: (Stack.HasCallStack, Exception.MonadThrow m) => [Flag.Flag] -> m Config
fromFlags = Monad.foldM applyFlag initial

applyFlag :: (Stack.HasCallStack, Exception.MonadThrow m) => Config -> Flag.Flag -> m Config
applyFlag config flag = case flag of
  Flag.Help maybeString -> case maybeString of
    Nothing -> pure config {help = True}
    Just string -> do
      bool <- Read.readM string
      pure config {help = bool}
  Flag.Literate maybeString -> case maybeString of
    Nothing -> pure config {literate = True}
    Just string -> do
      bool <- Read.readM string
      pure config {literate = bool}
  Flag.Schema maybeString -> case maybeString of
    Nothing -> pure config {schema = True}
    Just string -> do
      bool <- Read.readM string
      pure config {schema = bool}
  Flag.Signature maybeString -> case maybeString of
    Nothing -> pure config {signature = True}
    Just string -> do
      bool <- Read.readM string
      pure config {signature = bool}
  Flag.Version maybeString -> case maybeString of
    Nothing -> pure config {version = True}
    Just string -> do
      bool <- Read.readM string
      pure config {version = bool}

initial :: Config
initial =
  MkConfig
    { help = False,
      literate = False,
      schema = False,
      signature = False,
      version = False
    }

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = do
  Spec.named s 'fromFlags $ do
    Spec.it s "works with no flags" $ do
      Spec.assertEq s (fromFlags []) $ Just initial

    Spec.describe s "literate" $ do
      Spec.it s "works with nothing" $ do
        Spec.assertEq s (fromFlags [Flag.Literate Nothing]) $ Just initial {literate = True}

      Spec.it s "works with just false" $ do
        Spec.assertEq s (fromFlags [Flag.Literate $ Just "False"]) $ Just initial

      Spec.it s "works with just true" $ do
        Spec.assertEq s (fromFlags [Flag.Literate $ Just "True"]) $ Just initial {literate = True}

      Spec.it s "fails with just invalid" $ do
        Spec.assertEq s (fromFlags [Flag.Literate $ Just "invalid"]) Nothing

    Spec.describe s "schema" $ do
      Spec.it s "works with nothing" $ do
        Spec.assertEq s (fromFlags [Flag.Schema Nothing]) $ Just initial {schema = True}

      Spec.it s "works with just false" $ do
        Spec.assertEq s (fromFlags [Flag.Schema $ Just "False"]) $ Just initial

      Spec.it s "works with just true" $ do
        Spec.assertEq s (fromFlags [Flag.Schema $ Just "True"]) $ Just initial {schema = True}

      Spec.it s "fails with just invalid" $ do
        Spec.assertEq s (fromFlags [Flag.Schema $ Just "invalid"]) Nothing

    Spec.describe s "signature" $ do
      Spec.it s "works with nothing" $ do
        Spec.assertEq s (fromFlags [Flag.Signature Nothing]) $ Just initial {signature = True}

      Spec.it s "works with just false" $ do
        Spec.assertEq s (fromFlags [Flag.Signature $ Just "False"]) $ Just initial

      Spec.it s "works with just true" $ do
        Spec.assertEq s (fromFlags [Flag.Signature $ Just "True"]) $ Just initial {signature = True}

      Spec.it s "fails with just invalid" $ do
        Spec.assertEq s (fromFlags [Flag.Signature $ Just "invalid"]) Nothing

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
