{-# LANGUAGE TemplateHaskellQuotes #-}

module Scrod.Ghc.Unlit where

import qualified Data.List as List
import qualified Scrod.Spec as Spec

data Style
  = Bird
  | Latex
  deriving (Eq, Ord, Show)

detect :: String -> Maybe Style
detect input
  | elem "\\begin{code}" ls = Just Latex
  | any isBirdLine ls = Just Bird
  | otherwise = Nothing
  where
    ls = lines' input
    isBirdLine line = case line of
      '>' : ' ' : _ -> True
      ['>'] -> True
      _ -> False

preprocess :: String -> String
preprocess input = maybe id unlit (detect input) input

unlit :: Style -> String -> String
unlit style = case style of
  Bird -> unlitBird
  Latex -> unlitLatex

unlitBird :: String -> String
unlitBird =
  unlines' . fmap unlitBirdLine . lines'

unlitBirdLine :: String -> String
unlitBirdLine line = case line of
  '>' : ' ' : rest -> rest
  ['>'] -> ""
  _ -> ""

unlitLatex :: String -> String
unlitLatex =
  unlines' . unlitLatexLines False . lines'

unlitLatexLines :: Bool -> [String] -> [String]
unlitLatexLines _ [] = []
unlitLatexLines inCode (line : rest)
  | inCode =
      if line == "\\end{code}"
        then "" : unlitLatexLines False rest
        else line : unlitLatexLines True rest
  | line == "\\begin{code}" = "" : unlitLatexLines True rest
  | otherwise = "" : unlitLatexLines False rest

lines' :: String -> [String]
lines' string = case string of
  [] -> []
  _ -> case break (== '\n') string of
    (line, []) -> [line]
    (line, _ : rest) -> line : lines' rest

unlines' :: [String] -> String
unlines' = List.intercalate "\n"

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = do
  Spec.named s 'detect $ do
    Spec.it s "returns nothing for regular haskell" $ do
      Spec.assertEq s (detect "x = 1") Nothing

    Spec.it s "returns nothing for empty input" $ do
      Spec.assertEq s (detect "") Nothing

    Spec.it s "detects bird style" $ do
      Spec.assertEq s (detect "> x = 1") $ Just Bird

    Spec.it s "detects bird style with blank code line" $ do
      Spec.assertEq s (detect ">") $ Just Bird

    Spec.it s "detects latex style" $ do
      Spec.assertEq s (detect "\\begin{code}\nx = 1\n\\end{code}") $ Just Latex

    Spec.it s "prefers latex when both markers present" $ do
      Spec.assertEq s (detect "> x = 1\n\\begin{code}\ny = 2\n\\end{code}") $ Just Latex

  Spec.named s 'unlit $ do
    Spec.describe s "bird" $ do
      Spec.it s "converts a code line" $ do
        Spec.assertEq s (unlit Bird "> x = 1") "x = 1"

      Spec.it s "converts a blank code line" $ do
        Spec.assertEq s (unlit Bird ">") ""

      Spec.it s "removes a comment line" $ do
        Spec.assertEq s (unlit Bird "this is a comment") ""

      Spec.it s "handles multiple lines" $ do
        Spec.assertEq s (unlit Bird "> x = 1\n>\n> y = 2") "x = 1\n\ny = 2"

      Spec.it s "preserves line numbers" $ do
        Spec.assertEq s (unlit Bird "comment\n> x = 1") "\nx = 1"

      Spec.it s "handles empty input" $ do
        Spec.assertEq s (unlit Bird "") ""

    Spec.describe s "latex" $ do
      Spec.it s "converts a code block" $ do
        Spec.assertEq s (unlit Latex "\\begin{code}\nx = 1\n\\end{code}") "\nx = 1\n"

      Spec.it s "removes text outside code blocks" $ do
        Spec.assertEq s (unlit Latex "text\n\\begin{code}\nx = 1\n\\end{code}\nmore text") "\n\nx = 1\n\n"

      Spec.it s "handles multiple code blocks" $ do
        Spec.assertEq s (unlit Latex "\\begin{code}\nx = 1\n\\end{code}\ntext\n\\begin{code}\ny = 2\n\\end{code}") "\nx = 1\n\n\n\ny = 2\n"

      Spec.it s "handles empty input" $ do
        Spec.assertEq s (unlit Latex "") ""
