{-# LANGUAGE TemplateHaskellQuotes #-}

-- | CLI configuration record, built from parsed 'Flag.Flag' values.
module Scrod.Executable.Config where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Data.Sequence as Seq
import qualified GHC.Stack as Stack
import qualified Scrod.Executable.Flag as Flag
import qualified Scrod.Executable.Format as Format
import qualified Scrod.Extra.Read as Read
import qualified Scrod.Spec as Spec

data Config = MkConfig
  { format :: Format.Format,
    ghcOptions :: Seq.Seq String,
    guessExtensions :: Bool,
    help :: Bool,
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
  Flag.Format string -> do
    fmt <- Format.fromString string
    pure config {format = fmt}
  Flag.GhcOption string -> pure config {ghcOptions = ghcOptions config Seq.|> string}
  Flag.GuessExtensions maybeString -> case maybeString of
    Nothing -> pure config {guessExtensions = True}
    Just string -> do
      bool <- Read.readM string
      pure config {guessExtensions = bool}
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
    { format = Format.Json,
      ghcOptions = Seq.empty,
      guessExtensions = False,
      help = False,
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

    Spec.describe s "format" $ do
      Spec.it s "defaults to json" $ do
        Spec.assertEq s (fromFlags []) $ Just initial

      Spec.it s "works with json" $ do
        Spec.assertEq s (fromFlags [Flag.Format "json"]) $ Just initial

      Spec.it s "works with html" $ do
        Spec.assertEq s (fromFlags [Flag.Format "html"]) $ Just initial {format = Format.Html}

      Spec.it s "fails with invalid format" $ do
        Spec.assertEq s (fromFlags [Flag.Format "invalid"]) Nothing

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

    Spec.describe s "guessExtensions" $ do
      Spec.it s "works with nothing" $ do
        Spec.assertEq s (fromFlags [Flag.GuessExtensions Nothing]) $ Just initial {guessExtensions = True}

      Spec.it s "works with just false" $ do
        Spec.assertEq s (fromFlags [Flag.GuessExtensions $ Just "False"]) $ Just initial

      Spec.it s "works with just true" $ do
        Spec.assertEq s (fromFlags [Flag.GuessExtensions $ Just "True"]) $ Just initial {guessExtensions = True}

      Spec.it s "fails with just invalid" $ do
        Spec.assertEq s (fromFlags [Flag.GuessExtensions $ Just "invalid"]) Nothing

    Spec.describe s "ghcOptions" $ do
      Spec.it s "defaults to empty" $ do
        Spec.assertEq s (ghcOptions <$> fromFlags []) $ Just Seq.empty

      Spec.it s "collects one option" $ do
        Spec.assertEq s (ghcOptions <$> fromFlags [Flag.GhcOption "-XCPP"]) $ Just (Seq.fromList ["-XCPP"])

      Spec.it s "collects multiple options in order" $ do
        Spec.assertEq s (ghcOptions <$> fromFlags [Flag.GhcOption "-XCPP", Flag.GhcOption "-XGADTs"]) $ Just (Seq.fromList ["-XCPP", "-XGADTs"])
