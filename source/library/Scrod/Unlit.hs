{-# LANGUAGE TemplateHaskellQuotes #-}

module Scrod.Unlit where

import qualified Control.Monad as Monad
import qualified Data.Char as Char
import qualified Scrod.Spec as Spec

unlit :: String -> Either String String
unlit input = do
  let ls = lines input
  (hasCode, outputLines) <- go ls Blank False False []
  Monad.unless hasCode $ Left "no code in literate file"
  Right $ unlines (reverse outputLines)
  where
    go [] _ True _ _ = Left "unterminated \\begin{code}"
    go [] _ False hasCode acc = Right (hasCode, acc)
    go (line : rest) prev inLatex hasCode acc =
      let cls = classifyLine line
       in case inLatex of
            True -> case cls of
              EndCode -> go rest EndCode False hasCode ("" : acc)
              BeginCode -> Left "nested \\begin{code}"
              _ -> go rest cls True True (line : acc)
            False -> case cls of
              Blank -> go rest Blank False hasCode ("" : acc)
              Bird -> do
                Monad.when (prev == Comment) $ Left "bird track adjacent to comment"
                go rest Bird False True ((' ' : drop 1 line) : acc)
              BeginCode -> go rest BeginCode True True ("" : acc)
              EndCode -> Left "unexpected \\end{code}"
              Comment -> do
                Monad.when (prev == Bird) $ Left "bird track adjacent to comment"
                go rest Comment False hasCode ("" : acc)

data LineClass
  = Blank
  | Bird
  | BeginCode
  | EndCode
  | Comment
  deriving (Eq)

classifyLine :: String -> LineClass
classifyLine line
  | all Char.isSpace line = Blank
  | '>' : _ <- line = Bird
  | matchesDelimiter "\\begin{code}" line = BeginCode
  | matchesDelimiter "\\end{code}" line = EndCode
  | otherwise = Comment

matchesDelimiter :: String -> String -> Bool
matchesDelimiter delim line =
  let stripped = dropWhile Char.isSpace line
      (prefix, _) = splitAt (length delim) stripped
   in fmap Char.toLower prefix == delim

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = do
  Spec.named s 'unlit $ do
    Spec.it s "fails with no code" $ do
      Spec.assertEq s (unlit "") $ Left "no code in literate file"

    Spec.it s "fails with only comments" $ do
      Spec.assertEq s (unlit "hello") $ Left "no code in literate file"

    Spec.it s "works with empty latex block" $ do
      Spec.assertEq s (unlit "\\begin{code}\n\\end{code}") $ Right "\n\n"

    Spec.it s "works with bird style" $ do
      Spec.assertEq s (unlit "> x = 0") $ Right "  x = 0\n"

    Spec.it s "works with bird style without space" $ do
      Spec.assertEq s (unlit ">x = 0") $ Right " x = 0\n"

    Spec.it s "works with latex style" $ do
      Spec.assertEq s (unlit "\\begin{code}\nx = 0\n\\end{code}") $ Right "\nx = 0\n\n"

    Spec.describe s "bird adjacency" $ do
      Spec.it s "fails with comment before bird" $ do
        Spec.assertEq s (unlit "before\n> x = 0") $ Left "bird track adjacent to comment"

      Spec.it s "works with blank before bird" $ do
        Spec.assertEq s (unlit "before\n\n> x = 0") $ Right "\n\n  x = 0\n"

      Spec.it s "fails with comment after bird" $ do
        Spec.assertEq s (unlit "> x = 0\nafter") $ Left "bird track adjacent to comment"

      Spec.it s "works with blank after bird" $ do
        Spec.assertEq s (unlit "> x = 0\n\nafter") $ Right "  x = 0\n\n\n"

    Spec.describe s "latex blocks" $ do
      Spec.it s "fails with unclosed begin" $ do
        Spec.assertEq s (unlit "\\begin{code}") $ Left "unterminated \\begin{code}"

      Spec.it s "fails with extra end" $ do
        Spec.assertEq s (unlit "\\begin{code}\n\\end{code}\n\\end{code}") $ Left "unexpected \\end{code}"

      Spec.it s "fails with end without begin" $ do
        Spec.assertEq s (unlit "\\end{code}") $ Left "unexpected \\end{code}"

      Spec.it s "fails with nested begin" $ do
        Spec.assertEq s (unlit "\\begin{code}\n\\begin{code}") $ Left "nested \\begin{code}"

    Spec.describe s "latex indentation" $ do
      Spec.it s "works with indented delimiters" $ do
        Spec.assertEq s (unlit " \\begin{code}\nx = 0\n \\end{code}") $ Right "\nx = 0\n\n"

      Spec.it s "fails with spaces in delimiter" $ do
        Spec.assertEq s (unlit " \\ begin { code }\nx = 0\n \\end{code}") $ Left "unexpected \\end{code}"

    Spec.describe s "latex trailing text" $ do
      Spec.it s "works with text after delimiters" $ do
        Spec.assertEq s (unlit "\\begin{code} foo\nx = 0\n\\end{code} bar") $ Right "\nx = 0\n\n"

      Spec.it s "fails with text before delimiter" $ do
        Spec.assertEq s (unlit "foo \\begin{code}\nx = 0\nbar \\end{code}") $ Left "no code in literate file"

    Spec.it s "is case insensitive for latex" $ do
      Spec.assertEq s (unlit "\\BEGIN{CODE}\nx = 0\n\\END{CODE}") $ Right "\nx = 0\n\n"

    Spec.it s "converts comment lines to blank lines" $ do
      Spec.assertEq s (unlit "comment\n\n> x = 0\n\ncomment") $ Right "\n\n  x = 0\n\n\n"
