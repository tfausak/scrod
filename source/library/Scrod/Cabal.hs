{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Parse Cabal script headers and @.cabal@ files to discover extensions.
--
-- Supports two modes:
--
-- 1. __Cabal script headers__: block comments of the form @{- cabal: ... -}@
--    embedded in Haskell source files.
-- 2. __@.cabal@ files__: full package descriptions parsed via
--    @Cabal-syntax@'s 'PD.parseGenericPackageDescription'. Given a module
--    name, finds the component that owns the module and returns only that
--    component's extensions and language.
module Scrod.Cabal where

import qualified Data.ByteString.Char8 as Char8
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Distribution.Fields as Fields
import qualified Distribution.PackageDescription as PD
import qualified Distribution.PackageDescription.Configuration as PD
import qualified Distribution.PackageDescription.Parsec as PD
import qualified Distribution.Pretty as Pretty
import qualified Distribution.Types.Benchmark as Benchmark
import qualified Distribution.Types.BuildInfo as BuildInfo
import qualified Distribution.Types.Executable as Executable
import qualified Distribution.Types.Library as Library
import qualified Distribution.Types.TestSuite as TestSuite
import qualified GHC.Types.SrcLoc as SrcLoc
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

-- | Parse a full @.cabal@ file with component-aware matching.
--
-- Uses @Cabal-syntax@'s 'PD.parseGenericPackageDescription' to parse the
-- content into a 'PD.GenericPackageDescription', then
-- 'PD.flattenPackageDescription' to resolve conditional branches.
--
-- Given an optional module name, finds the component that owns the module
-- (checking @exposed-modules@, @other-modules@, and @signatures@) and
-- returns only that component's @default-extensions@ and
-- @default-language@.
--
-- Fallback strategy when no module matches:
--
-- 1. Use the main (unnamed) library component.
-- 2. Union of all components.
parseCabalFile :: Maybe String -> String -> ([String], Maybe String)
parseCabalFile moduleName content =
  case snd (PD.runParseResult (PD.parseGenericPackageDescription (Char8.pack content))) of
    Left _ -> ([], Nothing)
    Right gpd ->
      let pd = PD.flattenPackageDescription gpd
          target = findTargetBuildInfo moduleName pd
       in ( fmap Pretty.prettyShow (BuildInfo.defaultExtensions target),
            fmap Pretty.prettyShow (BuildInfo.defaultLanguage target)
          )

-- | Parse a @.cabal@ file and return GHC option strings for discovered
-- extensions and language, scoped to a specific module.
parseCabalFileOptionsForModule :: Maybe String -> String -> [SrcLoc.Located String]
parseCabalFileOptionsForModule moduleName content =
  let (exts, lang) = parseCabalFile moduleName content
      langOpts = foldMap (\l -> [SrcLoc.L SrcLoc.noSrcSpan $ "-X" <> l]) lang
      extOpts = fmap (\e -> SrcLoc.L SrcLoc.noSrcSpan $ "-X" <> e) exts
   in langOpts <> extOpts

-- | Find the 'BuildInfo' for the component that owns the given module.
findTargetBuildInfo :: Maybe String -> PD.PackageDescription -> BuildInfo.BuildInfo
findTargetBuildInfo moduleName pd =
  let allComps = collectComponents pd
      matched =
        moduleName >>= \name ->
          List.find (elem name . fst) allComps
   in case matched of
        Just (_, bi) -> bi
        Nothing -> case PD.library pd of
          Just lib -> Library.libBuildInfo lib
          Nothing -> mconcat (fmap snd allComps)

-- | Collect all components as @(module names, build info)@ pairs.
collectComponents :: PD.PackageDescription -> [([String], BuildInfo.BuildInfo)]
collectComponents pd =
  let libs = maybe id (:) (PD.library pd) (PD.subLibraries pd)
      libComps =
        [ ( fmap Pretty.prettyShow (Library.exposedModules lib <> Library.signatures lib)
              <> fmap Pretty.prettyShow (BuildInfo.otherModules (Library.libBuildInfo lib)),
            Library.libBuildInfo lib
          )
        | lib <- libs
        ]
      exeComps =
        [ ( fmap Pretty.prettyShow (BuildInfo.otherModules (Executable.buildInfo exe)),
            Executable.buildInfo exe
          )
        | exe <- PD.executables pd
        ]
      testComps =
        [ ( fmap Pretty.prettyShow (BuildInfo.otherModules (TestSuite.testBuildInfo ts)),
            TestSuite.testBuildInfo ts
          )
        | ts <- PD.testSuites pd
        ]
      benchComps =
        [ ( fmap Pretty.prettyShow (BuildInfo.otherModules (Benchmark.benchmarkBuildInfo bm)),
            Benchmark.benchmarkBuildInfo bm
          )
        | bm <- PD.benchmarks pd
        ]
   in libComps <> exeComps <> testComps <> benchComps

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

  Spec.named s 'parseCabalFile $ do
    let header :: String
        header = "cabal-version: 3.0\nname: test\nversion: 0\n"

    Spec.it s "returns empty for empty input" $ do
      Spec.assertEq s (parseCabalFile Nothing "") ([], Nothing)

    Spec.it s "returns empty for invalid cabal content" $ do
      Spec.assertEq s (parseCabalFile Nothing "!!!") ([], Nothing)

    Spec.it s "discovers extensions in library" $ do
      Spec.assertEq
        s
        ( parseCabalFile
            Nothing
            (header <> "library\n  default-extensions: GADTs")
        )
        (["GADTs"], Nothing)

    Spec.it s "discovers default-language" $ do
      Spec.assertEq
        s
        ( parseCabalFile
            Nothing
            (header <> "library\n  default-language: GHC2021")
        )
        ([], Just "GHC2021")

    Spec.it s "resolves common stanza imports" $ do
      Spec.assertEq
        s
        ( parseCabalFile
            Nothing
            (header <> "common warnings\n  default-extensions: OverloadedStrings\nlibrary\n  import: warnings\n  default-extensions: GADTs")
        )
        (["OverloadedStrings", "GADTs"], Nothing)

    Spec.it s "resolves chained common stanza imports" $ do
      Spec.assertEq
        s
        ( parseCabalFile
            Nothing
            (header <> "common base\n  default-language: Haskell2010\ncommon extended\n  import: base\n  default-extensions: GADTs\nlibrary\n  import: extended")
        )
        (["GADTs"], Just "Haskell2010")

    Spec.it s "picks the last default-language" $ do
      Spec.assertEq
        s
        ( parseCabalFile
            Nothing
            (header <> "common base\n  default-language: Haskell2010\nlibrary\n  import: base\n  default-language: GHC2021")
        )
        ([], Just "GHC2021")

    Spec.it s "discovers both extensions and language" $ do
      Spec.assertEq
        s
        ( parseCabalFile
            Nothing
            (header <> "library\n  default-language: GHC2021\n  default-extensions: OverloadedStrings")
        )
        (["OverloadedStrings"], Just "GHC2021")

    Spec.it s "matches module to library component" $ do
      Spec.assertEq
        s
        ( parseCabalFile
            (Just "Foo")
            (header <> "library\n  exposed-modules: Foo\n  default-extensions: GADTs\nexecutable bar\n  main-is: Main.hs\n  default-extensions: CPP")
        )
        (["GADTs"], Nothing)

    Spec.it s "matches module to executable component" $ do
      Spec.assertEq
        s
        ( parseCabalFile
            (Just "Bar")
            (header <> "library\n  exposed-modules: Foo\n  default-extensions: GADTs\nexecutable bar\n  main-is: Main.hs\n  other-modules: Bar\n  default-extensions: CPP")
        )
        (["CPP"], Nothing)

    Spec.it s "falls back to library when module not found" $ do
      Spec.assertEq
        s
        ( parseCabalFile
            (Just "Unknown")
            (header <> "library\n  exposed-modules: Foo\n  default-extensions: GADTs")
        )
        (["GADTs"], Nothing)
