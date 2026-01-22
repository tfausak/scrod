{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import qualified Scrod.Unstable.Type.Warning as Warning

spec :: forall m n. (Monad m, Monad n) => Test m n -> n ()
spec t = t.describe "extract" $ do
  let f :: (Stack.HasCallStack) => [String] -> m Interface.Interface
      f = expectRight t . Main.extract . unlines

  let h :: [String] -> Haddock.DocH Void.Void Haddock.Identifier
      h = Haddock._doc . Haddock.parseParas Nothing . unlines

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
      interface <- f []
      assertEq t interface.language Nothing

    t.it "handles language pragma" $ do
      interface <- f ["{-# language Haskell98 #-}"]
      assertEq t (interface.language <&> (.value)) $ Just "Haskell98"

    t.it "succeeds with Haskell2010" $ do
      interface <- f ["{-# language Haskell2010 #-}"]
      assertEq t (interface.language <&> (.value)) $ Just "Haskell2010"

    t.it "succeeds with GHC2021" $ do
      interface <- f ["{-# language GHC2021 #-}"]
      assertEq t (interface.language <&> (.value)) $ Just "GHC2021"

    t.it "succeeds with GHC2024" $ do
      interface <- f ["{-# language GHC2024 #-}"]
      assertEq t (interface.language <&> (.value)) $ Just "GHC2024"

    t.it "handles options_ghc pragma" $ do
      interface <- f ["{-# options_ghc -XHaskell98 #-}"]
      assertEq t (interface.language <&> (.value)) $ Just "Haskell98"

    t.it "handles options pragma" $ do
      interface <- f ["{-# options -XHaskell98 #-}"]
      assertEq t (interface.language <&> (.value)) $ Just "Haskell98"

    t.it "picks the last one in a pragma" $ do
      interface <- f ["{-# language Haskell98, Haskell2010 #-}"]
      assertEq t (interface.language <&> (.value)) $ Just "Haskell2010"

    t.it "picks the last pragma" $ do
      interface <- f ["{-# language Haskell98 #-}", "{-# language Haskell2010 #-}"]
      assertEq t (interface.language <&> (.value)) $ Just "Haskell2010"

  t.describe "extensions" $ do
    t.it "has no extensions by default" $ do
      interface <- f []
      assertEq t interface.extensions Map.empty

    t.it "handles language pragma" $ do
      interface <- f ["{-# language Arrows #-}"]
      assertEq t (interface.extensions & Map.mapKeys (.value)) $ Map.singleton "Arrows" True

    t.it "handles options_ghc pragma" $ do
      interface <- f ["{-# options_ghc -XArrows #-}"]
      assertEq t (interface.extensions & Map.mapKeys (.value)) $ Map.singleton "Arrows" True

    t.it "handles options pragma" $ do
      interface <- f ["{-# options -XArrows #-}"]
      assertEq t (interface.extensions & Map.mapKeys (.value)) $ Map.singleton "Arrows" True

    t.it "handles disabling an extension" $ do
      interface <- f ["{-# language NoArrows #-}"]
      assertEq t (interface.extensions & Map.mapKeys (.value)) $ Map.singleton "Arrows" False

    t.it "picks the last one in a pragma" $ do
      interface <- f ["{-# language Arrows, NoArrows #-}"]
      assertEq t (interface.extensions & Map.mapKeys (.value)) $ Map.singleton "Arrows" False

    t.it "picks the last pragma" $ do
      interface <- f ["{-# language Arrows #-}", "{-# language NoArrows #-}"]
      assertEq t (interface.extensions & Map.mapKeys (.value)) $ Map.singleton "Arrows" False

    t.it "handles unnecessarily enabling an extension" $ do
      interface <- f ["{-# language Haskell98, StarIsType #-}"]
      assertEq t (interface.extensions & Map.mapKeys (.value)) $ Map.singleton "StarIsType" True

    t.it "handles disabling an extension enabled by the language" $ do
      interface <- f ["{-# language Haskell98, NoStarIsType #-}"]
      assertEq t (interface.extensions & Map.mapKeys (.value)) $ Map.singleton "StarIsType" False

    t.it "disables an extension listed before the language" $ do
      interface <- f ["{-# language NoStarIsType, Haskell98 #-}"]
      assertEq t (interface.extensions & Map.mapKeys (.value)) $ Map.singleton "StarIsType" False

    t.it "handles implied extensions" $ do
      interface <- f ["{-# language PolyKinds #-}"]
      assertEq t (interface.extensions & Map.mapKeys (.value)) $
        Map.fromList
          [ ("KindSignatures", True),
            ("PolyKinds", True)
          ]

    t.it "supports Glasgow extensions" $ do
      interface <- f ["{-# options_ghc -fglasgow-exts #-}"]
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
      interface <- f []
      assertEq t interface.documentation Nothing

    t.it "works with block comment before" $ do
      interface <- f ["-- | x", "module M where"]
      assertEq t interface.documentation . Just $ h [" x"]

    t.it "works with inline comment before" $ do
      interface <- f ["{- | x -} module M where"]
      assertEq t interface.documentation . Just $ h [" x "]

    t.it "works with block comment after" $ do
      interface <- f ["module", "-- | x", "M where"]
      assertEq t interface.documentation . Just $ h [" x"]

    t.it "works with inline comment after" $ do
      interface <- f ["module {- | x -} M where"]
      assertEq t interface.documentation . Just $ h [" x "]

    t.it "works with multiple lines" $ do
      interface <- f ["-- | x", "-- y", "module M where"]
      assertEq t interface.documentation . Just $ h [" x", " y"]

    t.it "picks the first comment" $ do
      interface <- f ["-- | x", "module", "-- | y", "M where"]
      assertEq t interface.documentation . Just $ h [" x"]

    t.describe "markup" $ do
      t.it "missing" $ do
        interface <- f ["-- |", "module M where"]
        assertEq t interface.documentation Nothing

      t.it "empty" $ do
        interface <- f ["-- | ", "module M where"]
        assertEq t interface.documentation . Just $ h []

      t.it "string" $ do
        interface <- f ["-- | x", "module M where"]
        assertEq t interface.documentation . Just $ h [" x"]

      t.describe "identifier" $ do
        t.it "apostrophes" $ do
          interface <- f ["-- | 'x'", "module M where"]
          assertEq t interface.documentation . Just $ h [" 'x'"]

        t.it "grave apostrophe" $ do
          interface <- f ["-- | `x'", "module M where"]
          assertEq t interface.documentation . Just $ h [" `x'"]

        t.it "apostrophe grave" $ do
          interface <- f ["-- | 'x`", "module M where"]
          assertEq t interface.documentation . Just $ h [" 'x`"]

        t.it "graves" $ do
          interface <- f ["-- | `x`", "module M where"]
          assertEq t interface.documentation . Just $ h [" `x`"]

        t.it "qualified" $ do
          interface <- f ["-- | 'X.y'", "module M where"]
          assertEq t interface.documentation . Just $ h [" 'X.y'"]

        t.it "operator" $ do
          interface <- f ["-- | '(%)'", "module M where"]
          assertEq t interface.documentation . Just $ h [" '(%)'"]

        t.it "qualified operator" $ do
          interface <- f ["-- | '(X.%)'", "module M where"]
          assertEq t interface.documentation . Just $ h [" '(X.%)'"]

        t.it "value" $ do
          interface <- f ["-- | v'X'", "module M where"]
          assertEq t interface.documentation . Just $ h [" v'X'"]

        t.it "type" $ do
          interface <- f ["-- | t'X'", "module M where"]
          assertEq t interface.documentation . Just $ h [" t'X'"]

      t.describe "module" $ do
        t.it "without label" $ do
          interface <- f ["-- | \"X\"", "module M where"]
          assertEq t interface.documentation . Just $ h [" \"X\""]

        t.it "with label" $ do
          interface <- f ["-- | [X](\"Y\")", "module M where"]
          assertEq t interface.documentation . Just $ h [" [X](\"Y\")"]

      t.it "emphasis" $ do
        interface <- f ["-- | /x/", "module M where"]
        assertEq t interface.documentation . Just $ h [" /x/"]

      t.it "monospaced" $ do
        -- Note that the line can't be _only_ `@x@` because that would be a
        -- code block rather than some inline monospaced text.
        interface <- f ["-- | x @y@", "module M where"]
        assertEq t interface.documentation . Just $ h [" x @y@"]

      t.it "bold" $ do
        interface <- f ["-- | __x__", "module M where"]
        assertEq t interface.documentation . Just $ h [" __x__"]

      t.describe "unordered list" $ do
        t.it "asterisk" $ do
          interface <- f ["-- | * x", "module M where"]
          assertEq t interface.documentation . Just $ h [" * x"]

        t.it "hyphen" $ do
          interface <- f ["-- | - x", "module M where"]
          assertEq t interface.documentation . Just $ h [" - x"]

      t.describe "ordered list" $ do
        t.it "parentheses" $ do
          interface <- f ["-- | (1) x", "module M where"]
          assertEq t interface.documentation . Just $ h [" (1) x"]

        t.it "period" $ do
          interface <- f ["-- | 1. x", "module M where"]
          assertEq t interface.documentation . Just $ h [" 1. x"]

        t.it "custom number" $ do
          interface <- f ["-- | 2. x", "module M where"]
          assertEq t interface.documentation . Just $ h [" 2. x"]

      t.it "definition list" $ do
        interface <- f ["-- | [x]: y", "module M where"]
        assertEq t interface.documentation . Just $ h [" [x]: y"]

      t.it "code block" $ do
        interface <- f ["-- | @x@", "module M where"]
        assertEq t interface.documentation . Just $ h [" @x@"]

      t.describe "hyperlink" $ do
        t.it "implicit" $ do
          interface <- f ["-- | http://example", "module M where"]
          assertEq t interface.documentation . Just $ h [" http://example"]

        t.it "explicit" $ do
          interface <- f ["-- | <http://example>", "module M where"]
          assertEq t interface.documentation . Just $ h [" <http://example>"]

        t.it "with label" $ do
          interface <- f ["-- | [x](http://example)", "module M where"]
          assertEq t interface.documentation . Just $ h [" [x](http://example)"]

      t.it "pic" $ do
        interface <- f ["-- | ![x](http://example)", "module M where"]
        assertEq t interface.documentation . Just $ h [" ![x](http://example)"]

      t.describe "math" $ do
        t.it "inline" $ do
          interface <- f ["-- | \\(x\\)", "module M where"]
          assertEq t interface.documentation . Just $ h [" \\(x\\)"]

        t.it "display" $ do
          interface <- f ["-- | \\[x\\]", "module M where"]
          assertEq t interface.documentation . Just $ h [" \\[x\\]"]

      t.it "a name" $ do
        interface <- f ["-- | #x#", "module M where"]
        assertEq t interface.documentation . Just $ h [" #x#"]

      t.it "property" $ do
        interface <- f ["-- | prop> x", "module M where"]
        assertEq t interface.documentation . Just $ h [" prop> x"]

      t.describe "examples" $ do
        t.it "one" $ do
          interface <- f ["-- | >>> x", "module M where"]
          assertEq t interface.documentation . Just $ h [" >>> x"]

        t.it "two" $ do
          interface <- f ["-- | >>> x", "-- >>> y", "module M where"]
          assertEq t interface.documentation . Just $ h [" >>> x", " >>> y"]

        t.describe "results" $ do
          t.it "one" $ do
            interface <- f ["-- | >>> x", "-- y", "module M where"]
            assertEq t interface.documentation . Just $ h [" >>> x", " y"]

          t.it "two" $ do
            interface <- f ["-- | >>> x", "-- y", "-- z", "module M where"]
            assertEq t interface.documentation . Just $ h [" >>> x", " y", "z"]

          t.it "blank line" $ do
            interface <- f ["-- | >>> x", "-- <BLANKLINE>", "module M where"]
            assertEq t interface.documentation . Just $ h [" >>> x", " <BLANKLINE>"]

      t.describe "header" $ do
        t.it "one" $ do
          interface <- f ["-- | = x", "module M where"]
          assertEq t interface.documentation . Just $ h [" = x"]

        t.it "two" $ do
          interface <- f ["-- | == x", "module M where"]
          assertEq t interface.documentation . Just $ h [" == x"]

        t.it "three" $ do
          interface <- f ["-- | === x", "module M where"]
          assertEq t interface.documentation . Just $ h [" === x"]

        t.it "four" $ do
          interface <- f ["-- | ==== x", "module M where"]
          assertEq t interface.documentation . Just $ h [" ==== x"]

        t.it "five" $ do
          interface <- f ["-- | ===== x", "module M where"]
          assertEq t interface.documentation . Just $ h [" ===== x"]

        t.it "six" $ do
          interface <- f ["-- | ====== x", "module M where"]
          assertEq t interface.documentation . Just $ h [" ====== x"]

      t.describe "table" $ do
        let table :: (Stack.HasCallStack) => [String] -> m ()
            table xs = do
              interface <- f $ mconcat [["-- |"], fmap ("-- " <>) xs, ["module M where"]]
              assertEq t interface.documentation . Just $ h xs

        t.it "works" $ do
          table
            [ "+---+---+",
              "| a | b |",
              "+---+---+"
            ]

        t.it "with header" $ do
          table
            [ "+---+---+",
              "| a | b |",
              "+===+===+",
              "| c | d |",
              "+---+---+"
            ]

        t.it "with colspan" $ do
          table
            [ "+---+---+",
              "| a | b |",
              "+---+---+",
              "| c     |",
              "+---+---+"
            ]

        t.it "with rowspan" $ do
          table
            [ "+---+---+",
              "| a | b |",
              "+   +---+",
              "|   |   |",
              "+---+---+"
            ]

  t.describe "name" $ do
    t.it "has no module name by default" $ do
      interface <- f []
      assertEq t interface.name Nothing

    t.it "gets a simple name" $ do
      interface <- f ["module M where"]
      assertEq t (interface.name <&> (.value.value)) $ Just "M"

    t.it "gets a complex name" $ do
      interface <- f ["module M.N where"]
      assertEq t (interface.name <&> (.value.value)) $ Just "M.N"

    t.it "gets the line" $ do
      interface <- f ["module M where"]
      assertEq t (interface.name <&> (.location.line.value)) $ Just 1

    t.it "gets the column" $ do
      interface <- f ["module M where"]
      assertEq t (interface.name <&> (.location.column.value)) $ Just 8

  t.describe "warning" $ do
    t.it "has no warning by default" $ do
      interface <- f []
      assertEq t interface.warning Nothing

    t.describe "category" $ do
      t.it "uses deprecations for warning" $ do
        interface <- f ["module M {-# warning \"x\" #-} where"]
        assertEq t (interface.warning <&> (.category.value)) $ Just "deprecations"

      t.it "uses deprecations for deprecated" $ do
        interface <- f ["module M {-# deprecated \"x\" #-} where"]
        assertEq t (interface.warning <&> (.category.value)) $ Just "deprecations"

      t.it "works with x-custom" $ do
        interface <- f ["module M {-# warning in \"x-custom\" \"y\" #-} where"]
        assertEq t (interface.warning <&> (.category.value)) $ Just "x-custom"

    t.describe "value" $ do
      t.it "works with a string" $ do
        interface <- f ["module M {-# warning \"x\" #-} where"]
        assertEq t (interface.warning <&> (.value)) $ Just "x"

      t.it "works with an empty list" $ do
        interface <- f ["module M {-# warning [] #-} where"]
        assertEq t (interface.warning <&> (.value)) $ Just ""

      t.it "works with a singleton list" $ do
        interface <- f ["module M {-# warning [\"x\"] #-} where"]
        assertEq t (interface.warning <&> (.value)) $ Just "x"

      t.it "works with a list" $ do
        interface <- f ["module M {-# warning [\"x\", \"y\"] #-} where"]
        assertEq t (interface.warning <&> (.value)) $ Just "x\ny"
