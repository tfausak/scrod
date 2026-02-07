{-# LANGUAGE TemplateHaskellQuotes #-}

module Scrod.Executable.Flag where

import qualified Control.Monad.Catch as Exception
import qualified GHC.Stack as Stack
import qualified Scrod.Spec as Spec
import qualified System.Console.GetOpt as GetOpt

data Flag
  = Format String
  | Help (Maybe String)
  | Style String
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
    GetOpt.Option [] ["format"] (GetOpt.ReqArg Format "FORMAT") "Sets the output format (json or html).",
    GetOpt.Option [] ["style"] (GetOpt.ReqArg Style "STYLE") "Sets the literate Haskell style (bird, latex, or none)."
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

    Spec.describe s "format" $ do
      Spec.it s "works with an argument" $ do
        Spec.assertEq s (fromArguments ["--format=json"]) $ Just [Format "json"]

      Spec.it s "works with html" $ do
        Spec.assertEq s (fromArguments ["--format=html"]) $ Just [Format "html"]

      Spec.it s "fails with no argument" $ do
        Spec.assertEq s (fromArguments ["--format"]) Nothing

    Spec.describe s "help" $ do
      Spec.it s "works with no argument" $ do
        Spec.assertEq s (fromArguments ["--help"]) $ Just [Help Nothing]

      Spec.it s "works with an argument" $ do
        Spec.assertEq s (fromArguments ["--help="]) $ Just [Help $ Just ""]

    Spec.describe s "style" $ do
      Spec.it s "works with bird" $ do
        Spec.assertEq s (fromArguments ["--style=bird"]) $ Just [Style "bird"]

      Spec.it s "works with latex" $ do
        Spec.assertEq s (fromArguments ["--style=latex"]) $ Just [Style "latex"]

      Spec.it s "works with none" $ do
        Spec.assertEq s (fromArguments ["--style=none"]) $ Just [Style "none"]

      Spec.it s "fails with no argument" $ do
        Spec.assertEq s (fromArguments ["--style"]) Nothing

    Spec.describe s "version" $ do
      Spec.it s "works with no argument" $ do
        Spec.assertEq s (fromArguments ["--version"]) $ Just [Version Nothing]

      Spec.it s "works with an argument" $ do
        Spec.assertEq s (fromArguments ["--version="]) $ Just [Version $ Just ""]
