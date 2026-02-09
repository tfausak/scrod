{-# LANGUAGE TemplateHaskellQuotes #-}

module Scrod.Cpp where

import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Scrod.Cpp.Directive as Directive
import qualified Scrod.Cpp.Expr as Expr
import qualified Scrod.Spec as Spec

data Branch
  = Active
  | Inactive
  | Done
  deriving (Eq)

data State = MkState
  { defines :: Map.Map String String,
    stack :: [Branch],
    seenElse :: [Bool]
  }

initialState :: State
initialState =
  MkState
    { defines = Map.empty,
      stack = [],
      seenElse = []
    }

cpp :: String -> Either String String
cpp input = do
  let ls = lines input
  outputLines <- go ls initialState []
  Right $ unlines (reverse outputLines)

go :: [String] -> State -> [String] -> Either String [String]
go ls state acc = case ls of
  [] ->
    if null (stack state)
      then Right acc
      else Left "unterminated #if"
  line : rest ->
    case Directive.parse line of
      Nothing -> go rest state (keepOrBlank state line : acc)
      Just dir -> do
        state' <- processDirective state dir
        go rest state' ("" : acc)

isActive :: State -> Bool
isActive = isActiveStack . stack

isActiveStack :: [Branch] -> Bool
isActiveStack bs = case bs of
  [] -> True
  Active : _ -> True
  _ -> False

keepOrBlank :: State -> String -> String
keepOrBlank state line =
  if isActive state then line else ""

processDirective :: State -> Directive.Directive -> Either String State
processDirective state dir = case dir of
  Directive.If expr -> do
    if isActive state
      then do
        val <- Expr.evaluate (defines state) expr
        let branch = if val /= 0 then Active else Inactive
        Right state {stack = branch : stack state, seenElse = False : seenElse state}
      else
        Right state {stack = Inactive : stack state, seenElse = False : seenElse state}
  Directive.Ifdef name ->
    processDirective state (Directive.If $ "defined(" <> name <> ")")
  Directive.Ifndef name ->
    processDirective state (Directive.If $ "!defined(" <> name <> ")")
  Directive.Elif expr -> case (stack state, seenElse state) of
    ([], _) -> Left "unexpected #elif without matching #if"
    (_, True : _) -> Left "unexpected #elif after #else"
    (Active : rest, e : es) ->
      Right state {stack = Done : rest, seenElse = e : es}
    (Inactive : rest, e : es) ->
      if isActiveStack rest
        then do
          val <- Expr.evaluate (defines state) expr
          let branch = if val /= 0 then Active else Inactive
          Right state {stack = branch : rest, seenElse = e : es}
        else Right state {stack = Inactive : rest, seenElse = e : es}
    (Done : _, _ : _) ->
      Right state
    (_, []) -> Left "unexpected #elif without matching #if"
  Directive.Else -> case (stack state, seenElse state) of
    ([], _) -> Left "unexpected #else without matching #if"
    (_, True : _) -> Left "unexpected #else after #else"
    (Active : rest, _ : es) ->
      Right state {stack = Done : rest, seenElse = True : es}
    (Inactive : rest, _ : es) ->
      if isActiveStack rest
        then Right state {stack = Active : rest, seenElse = True : es}
        else Right state {stack = Inactive : rest, seenElse = True : es}
    (Done : _, _ : es) ->
      Right state {stack = stack state, seenElse = True : es}
    (_, []) -> Left "unexpected #else without matching #if"
  Directive.Endif -> case (stack state, seenElse state) of
    ([], _) -> Left "unexpected #endif without matching #if"
    (_ : rest, _ : es) -> Right state {stack = rest, seenElse = es}
    (_, []) -> Left "unexpected #endif without matching #if"
  Directive.Define name mValue ->
    if isActive state
      then
        let v = Maybe.fromMaybe "1" mValue
         in Right state {defines = Map.insert name v (defines state)}
      else Right state
  Directive.Undef name ->
    if isActive state
      then Right state {defines = Map.delete name (defines state)}
      else Right state
  Directive.Other -> Right state

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = do
  Spec.named s 'cpp $ do
    Spec.it s "passes through source without directives" $ do
      Spec.assertEq s (cpp "module M where") $ Right "module M where\n"

    Spec.it s "replaces directive lines with blank lines" $ do
      Spec.assertEq s (cpp "#define FOO\nmodule M where") $ Right "\nmodule M where\n"

    Spec.it s "keeps active ifdef branch" $ do
      Spec.assertEq s (cpp "#define FOO\n#ifdef FOO\nkeep\n#endif") $ Right "\n\nkeep\n\n"

    Spec.it s "removes inactive ifdef branch" $ do
      Spec.assertEq s (cpp "#ifdef FOO\nskip\n#endif") $ Right "\n\n\n"

    Spec.it s "handles ifndef" $ do
      Spec.assertEq s (cpp "#ifndef FOO\nkeep\n#endif") $ Right "\nkeep\n\n"

    Spec.it s "handles else branch" $ do
      Spec.assertEq s (cpp "#ifdef FOO\nskip\n#else\nkeep\n#endif") $ Right "\n\n\nkeep\n\n"

    Spec.it s "handles elif" $ do
      Spec.assertEq s (cpp "#if 0\nskip\n#elif 1\nkeep\n#endif") $ Right "\n\n\nkeep\n\n"

    Spec.it s "handles nested if" $ do
      Spec.assertEq s (cpp "#if 1\n#if 1\nkeep\n#endif\n#endif") $ Right "\n\nkeep\n\n\n"

    Spec.it s "handles nested if in inactive branch" $ do
      Spec.assertEq s (cpp "#if 0\n#if 1\nskip\n#endif\n#endif") $ Right "\n\n\n\n\n"

    Spec.it s "handles undef" $ do
      Spec.assertEq s (cpp "#define FOO\n#undef FOO\n#ifdef FOO\nskip\n#endif") $ Right "\n\n\n\n\n"

    Spec.it s "fails with unmatched endif" $ do
      Spec.assertEq s (cpp "#endif") $ Left "unexpected #endif without matching #if"

    Spec.it s "fails with unterminated if" $ do
      Spec.assertEq s (cpp "#if 1\ncode") $ Left "unterminated #if"

    Spec.it s "fails with else after else" $ do
      Spec.assertEq s (cpp "#if 1\n#else\n#else\n#endif") $ Left "unexpected #else after #else"

    Spec.it s "fails with elif after else" $ do
      Spec.assertEq s (cpp "#if 0\n#else\n#elif 1\n#endif") $ Left "unexpected #elif after #else"

    Spec.it s "handles else after elif" $ do
      Spec.assertEq s (cpp "#if 1\nkeep\n#elif 0\nskip\n#else\nskip\n#endif") $ Right "\nkeep\n\n\n\n\n\n"

    Spec.it s "preserves line count" $ do
      Spec.assertEq s (length . lines <$> cpp "#if 1\nline2\nline3\n#endif\nline5") $ Right 5

    Spec.it s "handles typical Haskell CPP pattern" $ do
      Spec.assertEq
        s
        ( cpp $
            unlines
              [ "#ifdef __GLASGOW_HASKELL__",
                "import GHC.Specific",
                "#else",
                "import Data.Old",
                "#endif"
              ]
        )
        ( Right $
            unlines
              [ "",
                "",
                "",
                "import Data.Old",
                ""
              ]
        )

    Spec.it s "handles define then ifdef" $ do
      Spec.assertEq
        s
        ( cpp $
            unlines
              [ "#define MY_FLAG",
                "#ifdef MY_FLAG",
                "import My.Module",
                "#endif"
              ]
        )
        ( Right $
            unlines
              [ "",
                "",
                "import My.Module",
                ""
              ]
        )

    Spec.it s "handles define with value in if" $ do
      Spec.assertEq s (cpp "#define VER 10\n#if VER >= 10\nkeep\n#endif") $ Right "\n\nkeep\n\n"

    Spec.it s "replaces other directives with blank lines" $ do
      Spec.assertEq s (cpp "#include <stdio.h>\nmodule M where") $ Right "\nmodule M where\n"
