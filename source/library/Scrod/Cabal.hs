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
--
-- The extraction logic mirrors @extractScriptBlock@ from
-- @cabal-install@'s @Distribution.Client.ScriptUtils@: both the
-- @{- cabal:@ start marker and the @-}@ end marker must appear on
-- their own lines (trailing whitespace is tolerated).
module Scrod.Cabal where

import qualified Data.ByteString.Char8 as Char8
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Distribution.Fields as Fields
import qualified Scrod.Spec as Spec

-- | Discover extension names from a Cabal script header, if present.
-- Returns extension names like @["TemplateHaskell", "GADTs"]@.
discoverExtensions :: String -> [String]
discoverExtensions source =
  foldMap parseDefaultExtensions (extractHeader source)

-- | Extract the content of a @{- cabal: ... -}@ block.
--
-- Matches @cabal-install@'s @extractScriptBlock@: the start marker
-- @{- cabal:@ and end marker @-}@ must each appear on their own line
-- (with optional trailing whitespace). Lines before the start marker
-- are skipped, so shebangs are handled naturally.
extractHeader :: String -> Maybe String
extractHeader = findStart . lines
  where
    findStart [] = Nothing
    findStart (l : ls)
      | isStartMarker l = collectBody [] ls
      | otherwise = findStart ls

    collectBody _ [] = Nothing
    collectBody acc (l : ls)
      | isEndMarker l = Just . unlines $ reverse acc
      | otherwise = collectBody (l : acc) ls

    isStartMarker = (== "{- cabal:") . stripTrailingSpace
    isEndMarker = (== "-}") . stripTrailingSpace

    stripTrailingSpace = List.dropWhileEnd Char.isSpace

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

    Spec.it s "returns empty when markers are not on their own lines" $ do
      Spec.assertEq
        s
        (discoverExtensions "{- cabal: default-extensions: TemplateHaskell -}")
        []

    Spec.it s "tolerates trailing whitespace on markers" $ do
      Spec.assertEq
        s
        (discoverExtensions "{- cabal:  \ndefault-extensions: TemplateHaskell\n-}  ")
        ["TemplateHaskell"]

  Spec.named s 'extractHeader $ do
    Spec.it s "returns Nothing for no header" $ do
      Spec.assertEq s (extractHeader "module Foo where") Nothing

    Spec.it s "returns Nothing for non-cabal block comment" $ do
      Spec.assertEq s (extractHeader "{- not cabal -}") Nothing

    Spec.it s "extracts header content" $ do
      Spec.assertEq s (extractHeader "{- cabal:\nbuild-depends: base\n-}") (Just "build-depends: base\n")

    Spec.it s "handles shebang" $ do
      Spec.assertEq s (extractHeader "#!/usr/bin/env cabal\n{- cabal:\nx\n-}") (Just "x\n")

    Spec.it s "returns Nothing for unclosed block" $ do
      Spec.assertEq s (extractHeader "{- cabal:\nbuild-depends: base") Nothing

    Spec.it s "skips non-marker lines before the header" $ do
      Spec.assertEq s (extractHeader "-- a comment\n{- cabal:\nx\n-}") (Just "x\n")
