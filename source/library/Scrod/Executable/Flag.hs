{-# LANGUAGE TemplateHaskellQuotes #-}

-- | CLI flag parsing via 'GetOpt'.
module Scrod.Executable.Flag where

import qualified Control.Monad.Catch as Exception
import qualified GHC.Stack as Stack
import qualified Scrod.Spec as Spec
import qualified System.Console.GetOpt as GetOpt

data Flag
  = Help (Maybe String)
  | Literate (Maybe String)
  | Schema (Maybe String)
  | Signature (Maybe String)
  | Version (Maybe String)
  deriving (Eq, Ord, Show)

fromArguments :: (Stack.HasCallStack, Exception.MonadThrow m) => [String] -> m [Flag]
fromArguments arguments = do
  let (flgs, args, opts, errs) = GetOpt.getOpt' GetOpt.Permute optDescrs arguments
  mapM_ (Exception.throwM . userError . mappend "invalid option: ") errs
  mapM_ (Exception.throwM . userError . mappend "unknown option: ") opts
  mapM_ (Exception.throwM . userError . mappend "unexpected argument: ") args
  pure flgs

optDescrs :: [GetOpt.OptDescr Flag]
optDescrs =
  [ GetOpt.Option ['h'] ["help"] (GetOpt.OptArg Help "BOOL") "Shows the help.",
    GetOpt.Option [] ["version"] (GetOpt.OptArg Version "BOOL") "Shows the version.",
    GetOpt.Option [] ["literate"] (GetOpt.OptArg Literate "BOOL") "Treats the input as Literate Haskell.",
    GetOpt.Option [] ["schema"] (GetOpt.OptArg Schema "BOOL") "Shows the JSON output schema.",
    GetOpt.Option [] ["signature"] (GetOpt.OptArg Signature "BOOL") "Treats the input as a Backpack signature."
  ]

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = do
  Spec.named s 'fromArguments $ do
    Spec.it s "works with no arguments" $ do
      Spec.assertEq s (fromArguments []) $ Just []

    Spec.it s "fails with an unknown option" $ do
      Spec.assertEq s (fromArguments ["-x"]) Nothing

    Spec.it s "fails with an unexpected argument" $ do
      Spec.assertEq s (fromArguments ["x"]) Nothing

    Spec.describe s "help" $ do
      Spec.it s "works with no argument" $ do
        Spec.assertEq s (fromArguments ["--help"]) $ Just [Help Nothing]

      Spec.it s "works with an argument" $ do
        Spec.assertEq s (fromArguments ["--help="]) $ Just [Help $ Just ""]

    Spec.describe s "literate" $ do
      Spec.it s "works with no argument" $ do
        Spec.assertEq s (fromArguments ["--literate"]) $ Just [Literate Nothing]

      Spec.it s "works with an argument" $ do
        Spec.assertEq s (fromArguments ["--literate="]) $ Just [Literate $ Just ""]

    Spec.describe s "schema" $ do
      Spec.it s "works with no argument" $ do
        Spec.assertEq s (fromArguments ["--schema"]) $ Just [Schema Nothing]

      Spec.it s "works with an argument" $ do
        Spec.assertEq s (fromArguments ["--schema="]) $ Just [Schema $ Just ""]

    Spec.describe s "signature" $ do
      Spec.it s "works with no argument" $ do
        Spec.assertEq s (fromArguments ["--signature"]) $ Just [Signature Nothing]

      Spec.it s "works with an argument" $ do
        Spec.assertEq s (fromArguments ["--signature="]) $ Just [Signature $ Just ""]

    Spec.describe s "version" $ do
      Spec.it s "works with no argument" $ do
        Spec.assertEq s (fromArguments ["--version"]) $ Just [Version Nothing]

      Spec.it s "works with an argument" $ do
        Spec.assertEq s (fromArguments ["--version="]) $ Just [Version $ Just ""]
