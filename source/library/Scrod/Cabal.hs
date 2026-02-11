{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Parse Cabal script headers to discover extensions.
--
-- Cabal scripts can specify @default-extensions@ in a block comment header
-- of the form:
--
-- > {- cabal:
-- > default-extensions: TemplateHaskell
-- > -}
--
-- This module extracts and parses that header using @Cabal-syntax@ to
-- discover extension names, which are then used during GHC parsing.
module Scrod.Cabal where

import qualified Data.ByteString.Char8 as Char8
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Distribution.Fields as Fields
import qualified Scrod.Spec as Spec

-- | Discover extension names from a Cabal script header, if present.
-- Returns extension names like @["TemplateHaskell", "GADTs"]@.
discoverExtensions :: String -> [String]
discoverExtensions source = case extractHeader source of
  Nothing -> []
  Just content -> parseDefaultExtensions content

-- | Extract the content of a @{- cabal: ... -}@ block comment header.
-- Returns the text between @cabal:@ and the matching @-}@.
extractHeader :: String -> Maybe String
extractHeader source = do
  let rest0 = skipShebang source
  let rest1 = dropWhile Char.isSpace rest0
  rest2 <- List.stripPrefix "{-" rest1
  let rest3 = dropWhile Char.isSpace rest2
  rest4 <- stripCaseInsensitivePrefix "cabal:" rest3
  (content, _) <- matchClose 0 rest4
  Just content

-- | Skip a shebang line (@#!@) if present at the start of the source.
skipShebang :: String -> String
skipShebang ('#' : '!' : rest) = drop 1 $ dropWhile (/= '\n') rest
skipShebang s = s

-- | Strip a case-insensitive prefix from a string.
stripCaseInsensitivePrefix :: String -> String -> Maybe String
stripCaseInsensitivePrefix [] s = Just s
stripCaseInsensitivePrefix _ [] = Nothing
stripCaseInsensitivePrefix (p : ps) (c : cs)
  | Char.toLower p == Char.toLower c = stripCaseInsensitivePrefix ps cs
  | otherwise = Nothing

-- | Find the matching @-}@ for a block comment, handling nested @{- -}@
-- pairs. Returns @(content, rest)@ where content is everything before the
-- closing @-}@ and rest is everything after.
matchClose :: Int -> String -> Maybe (String, String)
matchClose 0 ('-' : '}' : rest) = Just ([], rest)
matchClose n ('{' : '-' : rest) = do
  (inner, after) <- matchClose (n + 1) rest
  (more, final) <- matchClose n after
  Just ("{-" ++ inner ++ "-}" ++ more, final)
matchClose n ('-' : '}' : rest)
  | n > 0 = Just ([], rest)
matchClose n (c : rest) = do
  (content, after) <- matchClose n rest
  Just (c : content, after)
matchClose _ [] = Nothing

-- | Parse @default-extensions@ values from Cabal field content.
parseDefaultExtensions :: String -> [String]
parseDefaultExtensions content =
  case Fields.readFields (Char8.pack content) of
    Left _ -> []
    Right fields -> concatMap getExtensions fields
  where
    getExtensions :: Fields.Field pos -> [String]
    getExtensions (Fields.Field (Fields.Name _ name) fieldLines)
      | Char8.map Char.toLower name == Char8.pack "default-extensions" =
          concatMap getWords fieldLines
    getExtensions _ = []
    getWords :: Fields.FieldLine pos -> [String]
    getWords (Fields.FieldLine _ bs) =
      filter (not . null)
        . words
        . fmap (\c -> if c == ',' then ' ' else c)
        $ Char8.unpack bs

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = do
  Spec.named s 'discoverExtensions $ do
    Spec.it s "returns empty for no header" $ do
      Spec.assertEq s (discoverExtensions "module Foo where") []

    Spec.it s "returns empty for non-cabal block comment" $ do
      Spec.assertEq s (discoverExtensions "{- not cabal -}\nmodule Foo where") []

    Spec.it s "discovers a single extension" $ do
      Spec.assertEq
        s
        (discoverExtensions "{- cabal:\ndefault-extensions: TemplateHaskell\n-}")
        ["TemplateHaskell"]

    Spec.it s "discovers multiple extensions on separate lines" $ do
      Spec.assertEq
        s
        ( discoverExtensions
            "{- cabal:\ndefault-extensions:\n  TemplateHaskell\n  GADTs\n-}"
        )
        ["TemplateHaskell", "GADTs"]

    Spec.it s "discovers comma-separated extensions" $ do
      Spec.assertEq
        s
        (discoverExtensions "{- cabal:\ndefault-extensions: TemplateHaskell, GADTs\n-}")
        ["TemplateHaskell", "GADTs"]

    Spec.it s "handles shebang line" $ do
      Spec.assertEq
        s
        ( discoverExtensions
            "#!/usr/bin/env cabal\n{- cabal:\ndefault-extensions: TemplateHaskell\n-}"
        )
        ["TemplateHaskell"]

    Spec.it s "is case-insensitive on cabal prefix" $ do
      Spec.assertEq
        s
        (discoverExtensions "{- Cabal:\ndefault-extensions: TemplateHaskell\n-}")
        ["TemplateHaskell"]

  Spec.named s 'extractHeader $ do
    Spec.it s "returns Nothing for no header" $ do
      Spec.assertEq s (extractHeader "module Foo where") Nothing

    Spec.it s "returns Nothing for non-cabal block comment" $ do
      Spec.assertEq s (extractHeader "{- not cabal -}") Nothing

    Spec.it s "extracts header content" $ do
      Spec.assertEq s (extractHeader "{- cabal:\nbuild-depends: base\n-}") (Just "\nbuild-depends: base\n")

    Spec.it s "handles shebang" $ do
      Spec.assertEq s (extractHeader "#!/usr/bin/env cabal\n{- cabal: x -}") (Just " x ")

    Spec.it s "handles nested block comments" $ do
      Spec.assertEq s (extractHeader "{- cabal: {- nested -} rest -}") (Just " {- nested -} rest ")
