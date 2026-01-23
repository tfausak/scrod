{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Scrod.Unstable.Spec where

import qualified Data.Either as Either
import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Data.Map as Map
import qualified Data.Void as Void
import qualified Documentation.Haddock.Parser as Haddock
import qualified Documentation.Haddock.Types as Haddock
import qualified GHC.Stack as Stack
import Heck (Test, assertEq, describe, it)
import Scrod.Unstable.Extra.Heck (assertSatisfies, expectRight)
import qualified Scrod.Unstable.Main as Main
import qualified Scrod.Unstable.Type.Category as Category
import qualified Scrod.Unstable.Type.Column as Column
import qualified Scrod.Unstable.Type.Extension as Extension
import qualified Scrod.Unstable.Type.Interface as Interface
import qualified Scrod.Unstable.Type.Language as Language
import qualified Scrod.Unstable.Type.Line as Line
import qualified Scrod.Unstable.Type.Located as Located
import qualified Scrod.Unstable.Type.Location as Location
import qualified Scrod.Unstable.Type.ModuleName as ModuleName
import qualified Scrod.Unstable.Type.Since as Since
import qualified Scrod.Unstable.Type.Version as Version
import qualified Scrod.Unstable.Type.Warning as Warning

spec :: (Monad m, Monad n) => Test m n -> n ()
spec t = t.describe "extract" $ do
  t.describe "unsupported" $ do
    t.describe "lhs" $ do
      t.it "bird" $ do
        assertSatisfies t Either.isLeft $ Main.extract "> module M where"

      t.it "tex" $ do
        assertSatisfies t Either.isLeft $ Main.extract "\\begin{code}\nmodule M where\\end{code}"

    t.it "cpp" $ do
      assertSatisfies t Either.isLeft $ Main.extract "{-# language CPP #-}\n#line 1"

    t.it "hsig" $ do
      assertSatisfies t Either.isLeft $ Main.extract "signature S where"

  t.it "fails when there is a parse error" $ do
    assertSatisfies t Either.isLeft $ Main.extract "!"

  t.it "fails when there is an invalid language extension" $ do
    assertSatisfies t Either.isLeft $ Main.extract "{-# language x #-}"

  t.describe "language" $ do
    t.it "has no language by default" $ do
      interface <- scrod t []
      assertEq t interface.language Nothing

    t.it "handles language pragma" $ do
      interface <- scrod t ["{-# language Haskell98 #-}"]
      assertEq t (interface.language <&> (.value)) $ Just "Haskell98"

    t.it "succeeds with Haskell2010" $ do
      interface <- scrod t ["{-# language Haskell2010 #-}"]
      assertEq t (interface.language <&> (.value)) $ Just "Haskell2010"

    t.it "succeeds with GHC2021" $ do
      interface <- scrod t ["{-# language GHC2021 #-}"]
      assertEq t (interface.language <&> (.value)) $ Just "GHC2021"

    t.it "succeeds with GHC2024" $ do
      interface <- scrod t ["{-# language GHC2024 #-}"]
      assertEq t (interface.language <&> (.value)) $ Just "GHC2024"

    t.it "handles options_ghc pragma" $ do
      interface <- scrod t ["{-# options_ghc -XHaskell98 #-}"]
      assertEq t (interface.language <&> (.value)) $ Just "Haskell98"

    t.it "handles options pragma" $ do
      interface <- scrod t ["{-# options -XHaskell98 #-}"]
      assertEq t (interface.language <&> (.value)) $ Just "Haskell98"

    t.it "picks the last one in a pragma" $ do
      interface <- scrod t ["{-# language Haskell98, Haskell2010 #-}"]
      assertEq t (interface.language <&> (.value)) $ Just "Haskell2010"

    t.it "picks the last pragma" $ do
      interface <- scrod t ["{-# language Haskell98 #-}", "{-# language Haskell2010 #-}"]
      assertEq t (interface.language <&> (.value)) $ Just "Haskell2010"

  t.describe "extensions" $ do
    t.it "has no extensions by default" $ do
      interface <- scrod t []
      assertEq t interface.extensions Map.empty

    t.it "handles language pragma" $ do
      interface <- scrod t ["{-# language Arrows #-}"]
      assertEq t (interface.extensions & Map.mapKeys (.value)) $ Map.singleton "Arrows" True

    t.it "handles options_ghc pragma" $ do
      interface <- scrod t ["{-# options_ghc -XArrows #-}"]
      assertEq t (interface.extensions & Map.mapKeys (.value)) $ Map.singleton "Arrows" True

    t.it "handles options pragma" $ do
      interface <- scrod t ["{-# options -XArrows #-}"]
      assertEq t (interface.extensions & Map.mapKeys (.value)) $ Map.singleton "Arrows" True

    t.it "handles disabling an extension" $ do
      interface <- scrod t ["{-# language NoArrows #-}"]
      assertEq t (interface.extensions & Map.mapKeys (.value)) $ Map.singleton "Arrows" False

    t.it "picks the last one in a pragma" $ do
      interface <- scrod t ["{-# language Arrows, NoArrows #-}"]
      assertEq t (interface.extensions & Map.mapKeys (.value)) $ Map.singleton "Arrows" False

    t.it "picks the last pragma" $ do
      interface <- scrod t ["{-# language Arrows #-}", "{-# language NoArrows #-}"]
      assertEq t (interface.extensions & Map.mapKeys (.value)) $ Map.singleton "Arrows" False

    t.it "handles unnecessarily enabling an extension" $ do
      interface <- scrod t ["{-# language Haskell98, StarIsType #-}"]
      assertEq t (interface.extensions & Map.mapKeys (.value)) $ Map.singleton "StarIsType" True

    t.it "handles disabling an extension enabled by the language" $ do
      interface <- scrod t ["{-# language Haskell98, NoStarIsType #-}"]
      assertEq t (interface.extensions & Map.mapKeys (.value)) $ Map.singleton "StarIsType" False

    t.it "disables an extension listed before the language" $ do
      interface <- scrod t ["{-# language NoStarIsType, Haskell98 #-}"]
      assertEq t (interface.extensions & Map.mapKeys (.value)) $ Map.singleton "StarIsType" False

    t.it "handles implied extensions" $ do
      interface <- scrod t ["{-# language PolyKinds #-}"]
      assertEq t (interface.extensions & Map.mapKeys (.value)) $
        Map.fromList
          [ ("KindSignatures", True),
            ("PolyKinds", True)
          ]

    t.it "supports Glasgow extensions" $ do
      interface <- scrod t ["{-# options_ghc -fglasgow-exts #-}"]
      assertEq t (interface.extensions & Map.mapKeys (.value)) $
        Map.fromList
          [ ("ConstrainedClassMethods", True),
            ("DeriveDataTypeable", True),
            ("DeriveFoldable", True),
            ("DeriveFunctor", True),
            ("DeriveGeneric", True),
            ("DeriveTraversable", True),
            ("EmptyDataDecls", True),
            ("ExistentialQuantification", True),
            ("ExplicitForAll", True),
            ("ExplicitNamespaces", True),
            ("FlexibleContexts", True),
            ("FlexibleInstances", True),
            ("ForeignFunctionInterface", True),
            ("FunctionalDependencies", True),
            ("GeneralizedNewtypeDeriving", True),
            ("ImplicitParams", True),
            ("KindSignatures", True),
            ("LiberalTypeSynonyms", True),
            ("MagicHash", True),
            ("MultiParamTypeClasses", True),
            ("ParallelListComp", True),
            ("PatternGuards", True),
            ("PostfixOperators", True),
            ("RankNTypes", True),
            ("RecursiveDo", True),
            ("ScopedTypeVariables", True),
            ("StandaloneDeriving", True),
            ("TypeOperators", True),
            ("TypeSynonymInstances", True),
            ("UnboxedSums", True),
            ("UnboxedTuples", True),
            ("UnicodeSyntax", True),
            ("UnliftedFFITypes", True)
          ]

  t.describe "documentation" $ do
    t.it "has no documentation by default" $ do
      interface <- scrod t []
      assertEq t interface.documentation Haddock.DocEmpty

    t.it "works with block comment before" $ do
      interface <- scrod t ["-- | x", "module M where"]
      assertEq t interface.documentation $ haddock [" x"]

    t.it "works with inline comment before" $ do
      interface <- scrod t ["{- | x -} module M where"]
      assertEq t interface.documentation $ haddock [" x "]

    t.it "works with block comment after" $ do
      interface <- scrod t ["module", "-- | x", "M where"]
      assertEq t interface.documentation $ haddock [" x"]

    t.it "works with inline comment after" $ do
      interface <- scrod t ["module {- | x -} M where"]
      assertEq t interface.documentation $ haddock [" x "]

    t.it "works with multiple lines" $ do
      interface <- scrod t ["-- | x", "-- y", "module M where"]
      assertEq t interface.documentation $ haddock [" x", " y"]

    t.it "picks the first comment" $ do
      interface <- scrod t ["-- | x", "module", "-- | y", "M where"]
      assertEq t interface.documentation $ haddock [" x"]

    t.describe "markup" $ do
      t.it "missing" $ do
        interface <- scrod t ["-- |", "module M where"]
        assertEq t interface.documentation Haddock.DocEmpty

      t.it "empty" $ do
        interface <- scrod t ["-- | ", "module M where"]
        assertEq t interface.documentation $ haddock []

      t.it "string" $ do
        interface <- scrod t ["-- | x", "module M where"]
        assertEq t interface.documentation $ haddock [" x"]

      t.describe "identifier" $ do
        t.it "apostrophes" $ do
          interface <- scrod t ["-- | 'x'", "module M where"]
          assertEq t interface.documentation $ haddock [" 'x'"]

        t.it "grave apostrophe" $ do
          interface <- scrod t ["-- | `x'", "module M where"]
          assertEq t interface.documentation $ haddock [" `x'"]

        t.it "apostrophe grave" $ do
          interface <- scrod t ["-- | 'x`", "module M where"]
          assertEq t interface.documentation $ haddock [" 'x`"]

        t.it "graves" $ do
          interface <- scrod t ["-- | `x`", "module M where"]
          assertEq t interface.documentation $ haddock [" `x`"]

        t.it "qualified" $ do
          interface <- scrod t ["-- | 'X.y'", "module M where"]
          assertEq t interface.documentation $ haddock [" 'X.y'"]

        t.it "operator" $ do
          interface <- scrod t ["-- | '(%)'", "module M where"]
          assertEq t interface.documentation $ haddock [" '(%)'"]

        t.it "qualified operator" $ do
          interface <- scrod t ["-- | '(X.%)'", "module M where"]
          assertEq t interface.documentation $ haddock [" '(X.%)'"]

        t.it "value" $ do
          interface <- scrod t ["-- | v'X'", "module M where"]
          assertEq t interface.documentation $ haddock [" v'X'"]

        t.it "type" $ do
          interface <- scrod t ["-- | t'X'", "module M where"]
          assertEq t interface.documentation $ haddock [" t'X'"]

      t.describe "module" $ do
        t.it "without label" $ do
          interface <- scrod t ["-- | \"X\"", "module M where"]
          assertEq t interface.documentation $ haddock [" \"X\""]

        t.it "with label" $ do
          interface <- scrod t ["-- | [X](\"Y\")", "module M where"]
          assertEq t interface.documentation $ haddock [" [X](\"Y\")"]

      t.it "emphasis" $ do
        interface <- scrod t ["-- | /x/", "module M where"]
        assertEq t interface.documentation $ haddock [" /x/"]

      t.it "monospaced" $ do
        -- Note that the line can't be _only_ `@x@` because that would be a
        -- code block rather than some inline monospaced text.
        interface <- scrod t ["-- | x @y@", "module M where"]
        assertEq t interface.documentation $ haddock [" x @y@"]

      t.it "bold" $ do
        interface <- scrod t ["-- | __x__", "module M where"]
        assertEq t interface.documentation $ haddock [" __x__"]

      t.describe "unordered list" $ do
        t.it "asterisk" $ do
          interface <- scrod t ["-- | * x", "module M where"]
          assertEq t interface.documentation $ haddock [" * x"]

        t.it "hyphen" $ do
          interface <- scrod t ["-- | - x", "module M where"]
          assertEq t interface.documentation $ haddock [" - x"]

      t.describe "ordered list" $ do
        t.it "parentheses" $ do
          interface <- scrod t ["-- | (1) x", "module M where"]
          assertEq t interface.documentation $ haddock [" (1) x"]

        t.it "period" $ do
          interface <- scrod t ["-- | 1. x", "module M where"]
          assertEq t interface.documentation $ haddock [" 1. x"]

        t.it "custom number" $ do
          interface <- scrod t ["-- | 2. x", "module M where"]
          assertEq t interface.documentation $ haddock [" 2. x"]

      t.it "definition list" $ do
        interface <- scrod t ["-- | [x]: y", "module M where"]
        assertEq t interface.documentation $ haddock [" [x]: y"]

      t.describe "code block" $ do
        t.it "with inline formatting" $ do
          interface <- scrod t ["-- | @x@", "module M where"]
          assertEq t interface.documentation $ haddock [" @x@"]

        t.it "without inline formatting" $ do
          interface <- scrod t ["-- | > x", "module M where"]
          assertEq t interface.documentation $ haddock [" > x"]

      t.describe "hyperlink" $ do
        t.it "implicit" $ do
          interface <- scrod t ["-- | http://example", "module M where"]
          assertEq t interface.documentation $ haddock [" http://example"]

        t.it "explicit" $ do
          interface <- scrod t ["-- | <http://example>", "module M where"]
          assertEq t interface.documentation $ haddock [" <http://example>"]

        t.it "with label" $ do
          interface <- scrod t ["-- | [x](http://example)", "module M where"]
          assertEq t interface.documentation $ haddock [" [x](http://example)"]

      t.it "pic" $ do
        interface <- scrod t ["-- | ![x](http://example)", "module M where"]
        assertEq t interface.documentation $ haddock [" ![x](http://example)"]

      t.describe "math" $ do
        t.it "inline" $ do
          interface <- scrod t ["-- | \\(x\\)", "module M where"]
          assertEq t interface.documentation $ haddock [" \\(x\\)"]

        t.it "display" $ do
          interface <- scrod t ["-- | \\[x\\]", "module M where"]
          assertEq t interface.documentation $ haddock [" \\[x\\]"]

      t.it "a name" $ do
        interface <- scrod t ["-- | #x#", "module M where"]
        assertEq t interface.documentation $ haddock [" #x#"]

      t.it "property" $ do
        interface <- scrod t ["-- | prop> x", "module M where"]
        assertEq t interface.documentation $ haddock [" prop> x"]

      t.describe "examples" $ do
        t.it "one" $ do
          interface <- scrod t ["-- | >>> x", "module M where"]
          assertEq t interface.documentation $ haddock [" >>> x"]

        t.it "two" $ do
          interface <- scrod t ["-- | >>> x", "-- >>> y", "module M where"]
          assertEq t interface.documentation $ haddock [" >>> x", " >>> y"]

        t.describe "results" $ do
          t.it "one" $ do
            interface <- scrod t ["-- | >>> x", "-- y", "module M where"]
            assertEq t interface.documentation $ haddock [" >>> x", " y"]

          t.it "two" $ do
            interface <- scrod t ["-- | >>> x", "-- y", "-- z", "module M where"]
            assertEq t interface.documentation $ haddock [" >>> x", " y", "z"]

          t.it "blank line" $ do
            interface <- scrod t ["-- | >>> x", "-- <BLANKLINE>", "module M where"]
            assertEq t interface.documentation $ haddock [" >>> x", " <BLANKLINE>"]

      t.describe "header" $ do
        t.it "one" $ do
          interface <- scrod t ["-- | = x", "module M where"]
          assertEq t interface.documentation $ haddock [" = x"]

        t.it "two" $ do
          interface <- scrod t ["-- | == x", "module M where"]
          assertEq t interface.documentation $ haddock [" == x"]

        t.it "three" $ do
          interface <- scrod t ["-- | === x", "module M where"]
          assertEq t interface.documentation $ haddock [" === x"]

        t.it "four" $ do
          interface <- scrod t ["-- | ==== x", "module M where"]
          assertEq t interface.documentation $ haddock [" ==== x"]

        t.it "five" $ do
          interface <- scrod t ["-- | ===== x", "module M where"]
          assertEq t interface.documentation $ haddock [" ===== x"]

        t.it "six" $ do
          interface <- scrod t ["-- | ====== x", "module M where"]
          assertEq t interface.documentation $ haddock [" ====== x"]

      t.describe "table" $ do
        t.it "works" $ do
          table
            t
            [ "+---+---+",
              "| a | b |",
              "+---+---+"
            ]

        t.it "with header" $ do
          table
            t
            [ "+---+---+",
              "| a | b |",
              "+===+===+",
              "| c | d |",
              "+---+---+"
            ]

        t.it "with colspan" $ do
          table
            t
            [ "+---+---+",
              "| a | b |",
              "+---+---+",
              "| c     |",
              "+---+---+"
            ]

        t.it "with rowspan" $ do
          table
            t
            [ "+---+---+",
              "| a | b |",
              "+   +---+",
              "|   |   |",
              "+---+---+"
            ]

  t.describe "name" $ do
    t.it "has no module name by default" $ do
      interface <- scrod t []
      assertEq t interface.name Nothing

    t.it "gets a simple name" $ do
      interface <- scrod t ["module M where"]
      assertEq t (interface.name <&> (.value.value)) $ Just "M"

    t.it "gets a complex name" $ do
      interface <- scrod t ["module M.N where"]
      assertEq t (interface.name <&> (.value.value)) $ Just "M.N"

    t.it "gets the line" $ do
      interface <- scrod t ["module M where"]
      assertEq t (interface.name <&> (.location.line.value)) $ Just 1

    t.it "gets the column" $ do
      interface <- scrod t ["module M where"]
      assertEq t (interface.name <&> (.location.column.value)) $ Just 8

  t.describe "since" $ do
    t.it "is empty by default" $ do
      interface <- scrod t []
      assertEq t interface.since Since.empty

    t.describe "package" $ do
      t.it "works" $ do
        -- The code is in place to populate this field, but Haddock itself does
        -- not handle it. So if we update Haddock and this test starts failing,
        -- simply update the test.
        --
        -- > assertEq t (interface.since.package <&> (.value)) $ Just "p"
        interface <- scrod t ["-- | @since p-0", "module M where"]
        assertEq t interface.documentation $ haddock [" @since p-0"]

    t.describe "version" $ do
      t.it "parses a simple version" $ do
        interface <- scrod t ["-- | @since 0", "module M where"]
        assertEq t (interface.since.version <&> (.value)) $ Just [0]

      t.it "parses a complex version" $ do
        interface <- scrod t ["-- | @since 1.2", "module M where"]
        assertEq t (interface.since.version <&> (.value)) $ Just [1, 2]

  t.describe "warning" $ do
    t.it "has no warning by default" $ do
      interface <- scrod t []
      assertEq t interface.warning Nothing

    t.describe "category" $ do
      t.it "uses deprecations for warning" $ do
        interface <- scrod t ["module M {-# warning \"x\" #-} where"]
        assertEq t (interface.warning <&> (.category.value)) $ Just "deprecations"

      t.it "uses deprecations for deprecated" $ do
        interface <- scrod t ["module M {-# deprecated \"x\" #-} where"]
        assertEq t (interface.warning <&> (.category.value)) $ Just "deprecations"

      t.it "works with x-custom" $ do
        interface <- scrod t ["module M {-# warning in \"x-custom\" \"y\" #-} where"]
        assertEq t (interface.warning <&> (.category.value)) $ Just "x-custom"

    t.describe "value" $ do
      t.it "works with a string" $ do
        interface <- scrod t ["module M {-# warning \"x\" #-} where"]
        assertEq t (interface.warning <&> (.value)) $ Just "x"

      t.it "works with an empty list" $ do
        interface <- scrod t ["module M {-# warning [] #-} where"]
        assertEq t (interface.warning <&> (.value)) $ Just ""

      t.it "works with a singleton list" $ do
        interface <- scrod t ["module M {-# warning [\"x\"] #-} where"]
        assertEq t (interface.warning <&> (.value)) $ Just "x"

      t.it "works with a list" $ do
        interface <- scrod t ["module M {-# warning [\"x\", \"y\"] #-} where"]
        assertEq t (interface.warning <&> (.value)) $ Just "x\ny"

scrod :: (Stack.HasCallStack, Applicative m) => Test m n -> [String] -> m Interface.Interface
scrod t = expectRight t . Main.extract . unlines

haddock :: [String] -> Haddock.DocH Void.Void Haddock.Identifier
haddock = Haddock._doc . Haddock.parseParas Nothing . unlines

table :: (Stack.HasCallStack, Monad m) => Test m n -> [String] -> m ()
table t xs = do
  interface <- scrod t $ mconcat [["-- |"], fmap ("-- " <>) xs, ["module M where"]]
  assertEq t interface.documentation $ haddock xs
