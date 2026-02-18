{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Parse Cabal script headers and @.cabal@ files to discover extensions.
--
-- Supports two modes:
--
-- 1. __Cabal script headers__: block comments of the form @{- cabal: ... -}@
--    embedded in Haskell source files.
-- 2. __@.cabal@ files__: full package descriptions with component-aware
--    matching. Given a module name, finds the component that owns the module
--    and returns only that component's extensions and language (resolving
--    @common@ stanza imports).
module Scrod.Cabal where

import qualified Data.ByteString.Char8 as Char8
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Distribution.Fields as Fields
import qualified GHC.Types.SrcLoc as SrcLoc
import qualified Scrod.Spec as Spec

-- | A parsed component from a @.cabal@ file section.
data Component = MkComponent
  { componentModules :: [String],
    componentExtensions :: [String],
    componentLanguage :: Maybe String,
    componentImports :: [String]
  }
  deriving (Eq, Show)

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

-- | Extract a module name from Haskell source by scanning for
-- @module Foo.Bar@. Returns 'Nothing' if no module declaration is found.
extractModuleName :: String -> Maybe String
extractModuleName = go . words
  where
    go ("module" : (c : cs) : _)
      | Char.isUpper c = Just (takeWhile isModuleChar (c : cs))
    go (_ : rest) = go rest
    go [] = Nothing
    isModuleChar ch = Char.isAlphaNum ch || ch == '.' || ch == '_' || ch == '\''

-- | Parse a full @.cabal@ file with component-aware matching.
--
-- Given an optional module name, finds the component that owns the module
-- (checking @exposed-modules@, @other-modules@, and @signatures@) and
-- returns only that component's @default-extensions@ and
-- @default-language@ (with @common@ stanza imports resolved).
--
-- Fallback strategy when no module matches:
--
-- 1. Use the main (unnamed) library component.
-- 2. Union of all components and top-level fields.
parseCabalFile :: Maybe String -> String -> ([String], Maybe String)
parseCabalFile moduleName content =
  case Fields.readFields (Char8.pack content) of
    Left _ -> ([], Nothing)
    Right fields ->
      let (commons, mainLib, otherComps, topLevel) = categorizeFields fields
          resolvedLib = fmap (resolveImports commons) mainLib
          resolvedComps = fmap (resolveImports commons) otherComps
          allResolved = maybe id (:) resolvedLib resolvedComps
          matched = moduleName >>= \name -> List.find (elem name . componentModules) allResolved
          target = case matched of
            Just comp -> comp
            Nothing -> case resolvedLib of
              Just lib -> lib
              Nothing -> mergeComponents (topLevel : allResolved)
       in (componentExtensions target, componentLanguage target)

-- | Parse a @.cabal@ file and return GHC option strings for discovered
-- extensions and language, scoped to a specific module.
parseCabalFileOptionsForModule :: Maybe String -> String -> [SrcLoc.Located String]
parseCabalFileOptionsForModule moduleName content =
  let (exts, lang) = parseCabalFile moduleName content
      langOpts = foldMap (\l -> [SrcLoc.L SrcLoc.noSrcSpan $ "-X" <> l]) lang
      extOpts = fmap (\e -> SrcLoc.L SrcLoc.noSrcSpan $ "-X" <> e) exts
   in langOpts <> extOpts

-- | Parse a section's nested fields into a 'Component'.
parseComponent :: [Fields.Field pos] -> Component
parseComponent fields =
  let flat = flattenFields fields
      modules =
        concatMap (fieldValues "exposed-modules") flat
          <> concatMap (fieldValues "other-modules") flat
          <> concatMap (fieldValues "signatures") flat
      exts = concatMap (fieldValues "default-extensions") flat
      langs = concatMap (fieldValues "default-language") flat
      imports = concatMap (fieldValues "import") flat
   in MkComponent
        { componentModules = modules,
          componentExtensions = exts,
          componentLanguage = lastMaybe langs,
          componentImports = imports
        }

-- | Categorize top-level fields into common stanzas, the main library,
-- other components, and a top-level pseudo-component.
categorizeFields ::
  [Fields.Field pos] ->
  ( [(String, Component)],
    Maybe Component,
    [Component],
    Component
  )
categorizeFields fields =
  let (topFields, commons, mainLib, others) = go [] [] Nothing [] fields
   in (reverse commons, mainLib, reverse others, parseComponent (reverse topFields))
  where
    go :: [Fields.Field pos] -> [(String, Component)] -> Maybe Component -> [Component] -> [Fields.Field pos] -> ([Fields.Field pos], [(String, Component)], Maybe Component, [Component])
    go topFs cs lib os [] = (topFs, cs, lib, os)
    go topFs cs lib os (f : fs) = case f of
      Fields.Section (Fields.Name _ name) args nested
        | isName "common" name ->
            go topFs ((sectionArgName args, parseComponent nested) : cs) lib os fs
        | isName "library" name && null args ->
            go topFs cs (Just (parseComponent nested)) os fs
        | isComponentSection name ->
            go topFs cs lib (parseComponent nested : os) fs
      _ -> go (f : topFs) cs lib os fs

    isName target name = Char8.map Char.toLower name == Char8.pack target

    isComponentSection name =
      let lower = Char8.map Char.toLower name
       in lower
            `elem` fmap
              Char8.pack
              ["library", "executable", "test-suite", "benchmark", "foreign-library"]

    sectionArgName :: [Fields.SectionArg pos] -> String
    sectionArgName [] = ""
    sectionArgName (arg : _) = case arg of
      Fields.SecArgName _ bs -> Char8.unpack bs
      Fields.SecArgStr _ bs -> Char8.unpack bs
      Fields.SecArgOther _ bs -> Char8.unpack bs

-- | Resolve @import@ fields by merging common stanza extensions and
-- language into a component. Uses a visited list to prevent cycles.
resolveImports :: [(String, Component)] -> Component -> Component
resolveImports commons = go []
  where
    go visited comp =
      let newImports = filter (`notElem` visited) (componentImports comp)
          visited' = visited <> newImports
          imported =
            [ go visited' c
            | name <- newImports,
              (n, c) <- commons,
              n == name
            ]
          mergedExts = concatMap componentExtensions imported <> componentExtensions comp
          mergedLang = case componentLanguage comp of
            Just l -> Just l
            Nothing -> lastMaybe (Maybe.mapMaybe componentLanguage imported)
       in comp
            { componentExtensions = mergedExts,
              componentLanguage = mergedLang,
              componentImports = []
            }

-- | Merge multiple components into one (union of all extensions and modules).
mergeComponents :: [Component] -> Component
mergeComponents comps =
  MkComponent
    { componentModules = concatMap componentModules comps,
      componentExtensions = concatMap componentExtensions comps,
      componentLanguage = lastMaybe (Maybe.mapMaybe componentLanguage comps),
      componentImports = []
    }

getWords :: Fields.FieldLine pos -> [String]
getWords (Fields.FieldLine _ bs) =
  filter (not . null)
    . words
    . fmap (\c -> if c == ',' then ' ' else c)
    $ Char8.unpack bs

flattenFields :: [Fields.Field pos] -> [Fields.Field pos]
flattenFields = concatMap go
  where
    go :: Fields.Field pos -> [Fields.Field pos]
    go field@(Fields.Field _ _) = [field]
    go (Fields.Section _ _ nested) = concatMap go nested

fieldValues :: String -> Fields.Field pos -> [String]
fieldValues target (Fields.Field (Fields.Name _ name) fieldLines)
  | Char8.map Char.toLower name == Char8.pack target =
      concatMap getWords fieldLines
fieldValues _ _ = []

lastMaybe :: [a] -> Maybe a
lastMaybe [] = Nothing
lastMaybe xs = Just (List.last xs)

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

  Spec.named s 'extractModuleName $ do
    Spec.it s "extracts a simple module name" $ do
      Spec.assertEq s (extractModuleName "module Foo where") (Just "Foo")

    Spec.it s "extracts a dotted module name" $ do
      Spec.assertEq s (extractModuleName "module Foo.Bar.Baz where") (Just "Foo.Bar.Baz")

    Spec.it s "returns Nothing for empty input" $ do
      Spec.assertEq s (extractModuleName "") Nothing

    Spec.it s "returns Nothing for no module declaration" $ do
      Spec.assertEq s (extractModuleName "main = putStrLn \"hello\"") Nothing

    Spec.it s "handles Haddock comment before module" $ do
      Spec.assertEq s (extractModuleName "-- | Provides utilities\nmodule Foo where") (Just "Foo")

    Spec.it s "handles module on separate line from where" $ do
      Spec.assertEq s (extractModuleName "module Foo\n  where") (Just "Foo")

  Spec.named s 'parseCabalFile $ do
    Spec.it s "returns empty for empty input" $ do
      Spec.assertEq s (parseCabalFile Nothing "") ([], Nothing)

    Spec.it s "returns empty for invalid cabal content" $ do
      Spec.assertEq s (parseCabalFile Nothing "!!!") ([], Nothing)

    Spec.it s "discovers top-level default-extensions" $ do
      Spec.assertEq
        s
        (parseCabalFile Nothing "default-extensions: OverloadedStrings")
        (["OverloadedStrings"], Nothing)

    Spec.it s "discovers default-language" $ do
      Spec.assertEq
        s
        (parseCabalFile Nothing "default-language: GHC2021")
        ([], Just "GHC2021")

    Spec.it s "discovers extensions inside a section" $ do
      Spec.assertEq
        s
        ( parseCabalFile
            Nothing
            "library\n  default-extensions: GADTs"
        )
        (["GADTs"], Nothing)

    Spec.it s "resolves common stanza imports" $ do
      Spec.assertEq
        s
        ( parseCabalFile
            Nothing
            "common warnings\n  default-extensions: OverloadedStrings\nlibrary\n  import: warnings\n  default-extensions: GADTs"
        )
        (["OverloadedStrings", "GADTs"], Nothing)

    Spec.it s "resolves chained common stanza imports" $ do
      Spec.assertEq
        s
        ( parseCabalFile
            Nothing
            "common base\n  default-language: Haskell2010\ncommon extended\n  import: base\n  default-extensions: GADTs\nlibrary\n  import: extended"
        )
        (["GADTs"], Just "Haskell2010")

    Spec.it s "picks the last default-language" $ do
      Spec.assertEq
        s
        ( parseCabalFile
            Nothing
            "common base\n  default-language: Haskell2010\nlibrary\n  default-language: GHC2021"
        )
        ([], Just "GHC2021")

    Spec.it s "discovers both extensions and language" $ do
      Spec.assertEq
        s
        ( parseCabalFile
            Nothing
            "library\n  default-language: GHC2021\n  default-extensions: OverloadedStrings"
        )
        (["OverloadedStrings"], Just "GHC2021")

    Spec.it s "matches module to library component" $ do
      Spec.assertEq
        s
        ( parseCabalFile
            (Just "Foo")
            "library\n  exposed-modules: Foo\n  default-extensions: GADTs\nexecutable bar\n  default-extensions: CPP"
        )
        (["GADTs"], Nothing)

    Spec.it s "matches module to executable component" $ do
      Spec.assertEq
        s
        ( parseCabalFile
            (Just "Bar")
            "library\n  exposed-modules: Foo\n  default-extensions: GADTs\nexecutable bar\n  other-modules: Bar\n  default-extensions: CPP"
        )
        (["CPP"], Nothing)

    Spec.it s "falls back to library when module not found" $ do
      Spec.assertEq
        s
        ( parseCabalFile
            (Just "Unknown")
            "library\n  exposed-modules: Foo\n  default-extensions: GADTs"
        )
        (["GADTs"], Nothing)
