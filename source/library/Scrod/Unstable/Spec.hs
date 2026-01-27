{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -O0 #-}

module Scrod.Unstable.Spec where

import qualified Data.Either as Either
import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified GHC.Stack as Stack
import Heck (Test, assertEq, describe, it)
import Scrod.Unstable.Extra.Heck (assertSatisfies, expectRight)
import qualified Scrod.Unstable.Main as Main
import qualified Scrod.Unstable.Type.Category as Category
import qualified Scrod.Unstable.Type.Column as Column
import qualified Scrod.Unstable.Type.Doc as Doc
import qualified Scrod.Unstable.Type.Example as Example
import qualified Scrod.Unstable.Type.Export as Export
import qualified Scrod.Unstable.Type.ExportIdentifier as ExportIdentifier
import qualified Scrod.Unstable.Type.ExportName as ExportName
import qualified Scrod.Unstable.Type.ExportNameKind as ExportNameKind
import qualified Scrod.Unstable.Type.Extension as Extension
import qualified Scrod.Unstable.Type.Header as Header
import qualified Scrod.Unstable.Type.Hyperlink as Hyperlink
import qualified Scrod.Unstable.Type.Identifier as Identifier
import qualified Scrod.Unstable.Type.Interface as Interface
import qualified Scrod.Unstable.Type.Item as Item
import qualified Scrod.Unstable.Type.Language as Language
import qualified Scrod.Unstable.Type.Level as Level
import qualified Scrod.Unstable.Type.Line as Line
import qualified Scrod.Unstable.Type.Located as Located
import qualified Scrod.Unstable.Type.Location as Location
import qualified Scrod.Unstable.Type.ModLink as ModLink
import qualified Scrod.Unstable.Type.ModuleName as ModuleName
import qualified Scrod.Unstable.Type.Namespace as Namespace
import qualified Scrod.Unstable.Type.Picture as Picture
import qualified Scrod.Unstable.Type.Section as Section
import qualified Scrod.Unstable.Type.Since as Since
import qualified Scrod.Unstable.Type.Subordinates as Subordinates
import qualified Scrod.Unstable.Type.Table as Table
import qualified Scrod.Unstable.Type.TableCell as TableCell
import qualified Scrod.Unstable.Type.Version as Version
import qualified Scrod.Unstable.Type.Warning as Warning

spec :: (MonadFail m, Monad n) => Test m n -> n ()
spec t = describe t "extract" $ do
  describe t "unsupported" $ do
    describe t "lhs" $ do
      it t "bird" $ do
        assertSatisfies t Either.isLeft $ Main.extract "> module M where"

      it t "tex" $ do
        assertSatisfies t Either.isLeft $ Main.extract "\\begin{code}\nmodule M where\\end{code}"

    it t "cpp" $ do
      assertSatisfies t Either.isLeft $ Main.extract "{-# language CPP #-}\n#line 1"

    it t "hsig" $ do
      assertSatisfies t Either.isLeft $ Main.extract "signature S where"

  it t "fails when there is a parse error" $ do
    assertSatisfies t Either.isLeft $ Main.extract "!"

  it t "fails when there is an invalid language extension" $ do
    assertSatisfies t Either.isLeft $ Main.extract "{-# language x #-}"

  describe t "language" $ do
    it t "has no language by default" $ do
      interface <- scrod t []
      assertEq t interface.language Nothing

    it t "handles language pragma" $ do
      interface <- scrod t ["{-# language Haskell98 #-}"]
      assertEq t (interface.language <&> (.value)) $ Just "Haskell98"

    it t "succeeds with Haskell2010" $ do
      interface <- scrod t ["{-# language Haskell2010 #-}"]
      assertEq t (interface.language <&> (.value)) $ Just "Haskell2010"

    it t "succeeds with GHC2021" $ do
      interface <- scrod t ["{-# language GHC2021 #-}"]
      assertEq t (interface.language <&> (.value)) $ Just "GHC2021"

    it t "succeeds with GHC2024" $ do
      interface <- scrod t ["{-# language GHC2024 #-}"]
      assertEq t (interface.language <&> (.value)) $ Just "GHC2024"

    it t "handles options_ghc pragma" $ do
      interface <- scrod t ["{-# options_ghc -XHaskell98 #-}"]
      assertEq t (interface.language <&> (.value)) $ Just "Haskell98"

    it t "handles options pragma" $ do
      interface <- scrod t ["{-# options -XHaskell98 #-}"]
      assertEq t (interface.language <&> (.value)) $ Just "Haskell98"

    it t "picks the last one in a pragma" $ do
      interface <- scrod t ["{-# language Haskell98, Haskell2010 #-}"]
      assertEq t (interface.language <&> (.value)) $ Just "Haskell2010"

    it t "picks the last pragma" $ do
      interface <- scrod t ["{-# language Haskell98 #-}", "{-# language Haskell2010 #-}"]
      assertEq t (interface.language <&> (.value)) $ Just "Haskell2010"

  describe t "extensions" $ do
    it t "has no extensions by default" $ do
      interface <- scrod t []
      assertEq t interface.extensions Map.empty

    it t "handles language pragma" $ do
      interface <- scrod t ["{-# language Arrows #-}"]
      assertEq t (interface.extensions & Map.mapKeys (.value)) $ Map.singleton "Arrows" True

    it t "handles options_ghc pragma" $ do
      interface <- scrod t ["{-# options_ghc -XArrows #-}"]
      assertEq t (interface.extensions & Map.mapKeys (.value)) $ Map.singleton "Arrows" True

    it t "handles options pragma" $ do
      interface <- scrod t ["{-# options -XArrows #-}"]
      assertEq t (interface.extensions & Map.mapKeys (.value)) $ Map.singleton "Arrows" True

    it t "handles disabling an extension" $ do
      interface <- scrod t ["{-# language NoArrows #-}"]
      assertEq t (interface.extensions & Map.mapKeys (.value)) $ Map.singleton "Arrows" False

    it t "picks the last one in a pragma" $ do
      interface <- scrod t ["{-# language Arrows, NoArrows #-}"]
      assertEq t (interface.extensions & Map.mapKeys (.value)) $ Map.singleton "Arrows" False

    it t "picks the last pragma" $ do
      interface <- scrod t ["{-# language Arrows #-}", "{-# language NoArrows #-}"]
      assertEq t (interface.extensions & Map.mapKeys (.value)) $ Map.singleton "Arrows" False

    it t "handles unnecessarily enabling an extension" $ do
      interface <- scrod t ["{-# language Haskell98, StarIsType #-}"]
      assertEq t (interface.extensions & Map.mapKeys (.value)) $ Map.singleton "StarIsType" True

    it t "handles disabling an extension enabled by the language" $ do
      interface <- scrod t ["{-# language Haskell98, NoStarIsType #-}"]
      assertEq t (interface.extensions & Map.mapKeys (.value)) $ Map.singleton "StarIsType" False

    it t "disables an extension listed before the language" $ do
      interface <- scrod t ["{-# language NoStarIsType, Haskell98 #-}"]
      assertEq t (interface.extensions & Map.mapKeys (.value)) $ Map.singleton "StarIsType" False

    it t "handles implied extensions" $ do
      interface <- scrod t ["{-# language PolyKinds #-}"]
      assertEq t (interface.extensions & Map.mapKeys (.value)) $
        Map.fromList
          [ ("KindSignatures", True),
            ("PolyKinds", True)
          ]

    it t "supports Glasgow extensions" $ do
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

    describe t "special cases" $ do
      it t "cpp" $ do
        interface <- scrod t ["{-# language CPP #-}"]
        -- Ensuring we don't get "Cpp", which is the name of the constructor.
        assertEq t (interface.extensions & Map.mapKeys (.value)) $ Map.singleton "CPP" True

      it t "recursive do" $ do
        interface <- scrod t ["{-# language RecursiveDo #-}"]
        -- Ensuring we don't get "DoRec", which is a deprecated alias.
        assertEq t (interface.extensions & Map.mapKeys (.value)) $ Map.singleton "RecursiveDo" True

      it t "named field puns" $ do
        interface <- scrod t ["{-# language NamedFieldPuns #-}"]
        -- Ensuring we don't get "RecordPuns", which is a deprecated alias.
        assertEq t (interface.extensions & Map.mapKeys (.value)) $ Map.singleton "NamedFieldPuns" True

      it t "rank n types" $ do
        interface <- scrod t ["{-# language RankNTypes #-}"]
        -- Ensuring we don't get "PolymorphicComponents" or "Rank2Types", which
        -- are alternative names.
        assertEq t (interface.extensions & Map.mapKeys (.value)) $
          Map.fromList
            [ ("ExplicitForAll", True),
              ("RankNTypes", True)
            ]

  describe t "documentation" $ do
    it t "has no documentation by default" $ do
      interface <- scrod t []
      assertEq t interface.documentation Doc.Empty

    it t "works with block comment before" $ do
      interface <- scrod t ["-- | x", "module M where"]
      assertEq t interface.documentation . Doc.Paragraph $ Doc.String "x"

    it t "works with inline comment before" $ do
      interface <- scrod t ["{- | x -} module M where"]
      assertEq t interface.documentation . Doc.Paragraph $ Doc.String "x "

    it t "works with block comment after" $ do
      interface <- scrod t ["module", "-- | x", "M where"]
      assertEq t interface.documentation . Doc.Paragraph $ Doc.String "x"

    it t "works with inline comment after" $ do
      interface <- scrod t ["module {- | x -} M where"]
      assertEq t interface.documentation . Doc.Paragraph $ Doc.String "x "

    it t "works with multiple lines" $ do
      interface <- scrod t ["-- | x", "-- y", "module M where"]
      assertEq t interface.documentation . Doc.Paragraph $ Doc.String "x\n y"

    it t "picks the first comment" $ do
      interface <- scrod t ["-- | x", "module", "-- | y", "M where"]
      assertEq t interface.documentation . Doc.Paragraph $ Doc.String "x"

    describe t "markup" $ do
      it t "missing" $ do
        interface <- scrod t ["-- |", "module M where"]
        assertEq t interface.documentation Doc.Empty

      it t "empty" $ do
        interface <- scrod t ["-- | ", "module M where"]
        assertEq t interface.documentation Doc.Empty

      it t "string" $ do
        interface <- scrod t ["-- | x", "module M where"]
        assertEq t interface.documentation . Doc.Paragraph $ Doc.String "x"

      describe t "identifier" $ do
        it t "apostrophes" $ do
          interface <- scrod t ["-- | 'x'", "module M where"]
          Doc.Paragraph (Doc.Identifier identifier) <- pure interface.documentation
          assertEq t identifier.value "x"

        it t "grave apostrophe" $ do
          interface <- scrod t ["-- | `x'", "module M where"]
          Doc.Paragraph (Doc.Identifier identifier) <- pure interface.documentation
          assertEq t identifier.value "x"

        it t "apostrophe grave" $ do
          interface <- scrod t ["-- | 'x`", "module M where"]
          Doc.Paragraph (Doc.Identifier identifier) <- pure interface.documentation
          assertEq t identifier.value "x"

        it t "graves" $ do
          interface <- scrod t ["-- | `x`", "module M where"]
          Doc.Paragraph (Doc.Identifier identifier) <- pure interface.documentation
          assertEq t identifier.value "x"

        it t "qualified" $ do
          interface <- scrod t ["-- | 'X.y'", "module M where"]
          Doc.Paragraph (Doc.Identifier identifier) <- pure interface.documentation
          assertEq t identifier.value "X.y"

        it t "operator" $ do
          interface <- scrod t ["-- | '(%)'", "module M where"]
          Doc.Paragraph (Doc.Identifier identifier) <- pure interface.documentation
          assertEq t identifier.value "(%)"

        it t "qualified operator" $ do
          interface <- scrod t ["-- | '(X.%)'", "module M where"]
          Doc.Paragraph (Doc.Identifier identifier) <- pure interface.documentation
          assertEq t identifier.value "(X.%)"

        it t "value" $ do
          interface <- scrod t ["-- | v'X'", "module M where"]
          Doc.Paragraph (Doc.Identifier identifier) <- pure interface.documentation
          assertEq t identifier.namespace $ Just Namespace.Value

        it t "type" $ do
          interface <- scrod t ["-- | t'X'", "module M where"]
          Doc.Paragraph (Doc.Identifier identifier) <- pure interface.documentation
          assertEq t identifier.namespace $ Just Namespace.Type

      describe t "module" $ do
        it t "without label" $ do
          interface <- scrod t ["-- | \"X\"", "module M where"]
          Doc.Paragraph (Doc.Module modLink) <- pure interface.documentation
          assertEq t modLink.name.value "X"

        it t "with label" $ do
          interface <- scrod t ["-- | [X](\"Y\")", "module M where"]
          Doc.Paragraph (Doc.Module modLink) <- pure interface.documentation
          assertEq t modLink.label . Just $ Doc.String "X"

      it t "emphasis" $ do
        interface <- scrod t ["-- | /x/", "module M where"]
        assertEq t interface.documentation . Doc.Paragraph . Doc.Emphasis $ Doc.String "x"

      it t "monospaced" $ do
        -- Note that the line can't be _only_ `@x@` because that would be a
        -- code block rather than some inline monospaced text.
        interface <- scrod t ["-- | x @y@", "module M where"]
        assertEq t interface.documentation . Doc.Paragraph . Doc.Append (Doc.String "x ") . Doc.Monospaced $ Doc.String "y"

      it t "bold" $ do
        interface <- scrod t ["-- | __x__", "module M where"]
        assertEq t interface.documentation . Doc.Paragraph . Doc.Bold $ Doc.String "x"

      describe t "unordered list" $ do
        it t "asterisk" $ do
          interface <- scrod t ["-- | * x", "module M where"]
          assertEq t interface.documentation $ Doc.UnorderedList [Doc.Paragraph $ Doc.String "x"]

        it t "hyphen" $ do
          interface <- scrod t ["-- | - x", "module M where"]
          assertEq t interface.documentation $ Doc.UnorderedList [Doc.Paragraph $ Doc.String "x"]

      describe t "ordered list" $ do
        it t "parentheses" $ do
          interface <- scrod t ["-- | (1) x", "module M where"]
          assertEq t interface.documentation $ Doc.OrderedList [(1, Doc.Paragraph $ Doc.String "x")]

        it t "period" $ do
          interface <- scrod t ["-- | 1. x", "module M where"]
          assertEq t interface.documentation $ Doc.OrderedList [(1, Doc.Paragraph $ Doc.String "x")]

        it t "custom number" $ do
          interface <- scrod t ["-- | 2. x", "module M where"]
          assertEq t interface.documentation $ Doc.OrderedList [(2, Doc.Paragraph $ Doc.String "x")]

      it t "definition list" $ do
        interface <- scrod t ["-- | [x]: y", "module M where"]
        assertEq t interface.documentation $ Doc.DefList [(Doc.String "x", Doc.String "y")]

      describe t "code block" $ do
        it t "with inline formatting" $ do
          interface <- scrod t ["-- | @x@", "module M where"]
          assertEq t interface.documentation . Doc.CodeBlock $ Doc.String "x"

        it t "without inline formatting" $ do
          interface <- scrod t ["-- | > x", "module M where"]
          assertEq t interface.documentation . Doc.CodeBlock $ Doc.String "x"

      describe t "hyperlink" $ do
        it t "implicit" $ do
          interface <- scrod t ["-- | http://example", "module M where"]
          Doc.Paragraph (Doc.Hyperlink hyperlink) <- pure interface.documentation
          assertEq t hyperlink.url "http://example"

        it t "explicit" $ do
          interface <- scrod t ["-- | <http://example>", "module M where"]
          Doc.Paragraph (Doc.Hyperlink hyperlink) <- pure interface.documentation
          assertEq t hyperlink.url "http://example"

        it t "with label" $ do
          interface <- scrod t ["-- | [x](http://example)", "module M where"]
          Doc.Paragraph (Doc.Hyperlink hyperlink) <- pure interface.documentation
          assertEq t hyperlink.label . Just $ Doc.String "x"

      describe t "pic" $ do
        it t "uri" $ do
          interface <- scrod t ["-- | ![x](http://example)", "module M where"]
          Doc.Paragraph (Doc.Pic picture) <- pure interface.documentation
          assertEq t picture.uri "http://example"

        it t "title" $ do
          interface <- scrod t ["-- | ![x](http://example)", "module M where"]
          Doc.Paragraph (Doc.Pic picture) <- pure interface.documentation
          assertEq t picture.title $ Just "x"

      describe t "math" $ do
        it t "inline" $ do
          interface <- scrod t ["-- | \\(x\\)", "module M where"]
          assertEq t interface.documentation . Doc.Paragraph $ Doc.MathInline "x"

        it t "display" $ do
          interface <- scrod t ["-- | \\[x\\]", "module M where"]
          assertEq t interface.documentation . Doc.Paragraph $ Doc.MathDisplay "x"

      it t "a name" $ do
        interface <- scrod t ["-- | #x#", "module M where"]
        assertEq t interface.documentation . Doc.Paragraph $ Doc.AName "x"

      it t "property" $ do
        interface <- scrod t ["-- | prop> x", "module M where"]
        assertEq t interface.documentation $ Doc.Property "x"

      describe t "examples" $ do
        it t "one" $ do
          interface <- scrod t ["-- | >>> x", "module M where"]
          Doc.Examples examples <- pure interface.documentation
          assertEq t (examples <&> (.expression)) ["x"]

        it t "two" $ do
          interface <- scrod t ["-- | >>> x", "-- >>> y", "module M where"]
          Doc.Examples examples <- pure interface.documentation
          assertEq t (examples <&> (.expression)) ["x", "y"]

        describe t "results" $ do
          it t "one" $ do
            interface <- scrod t ["-- | >>> x", "-- y", "module M where"]
            Doc.Examples examples <- pure interface.documentation
            assertEq t (examples <&> (.result)) [["y"]]

          it t "two" $ do
            interface <- scrod t ["-- | >>> x", "-- y", "-- z", "module M where"]
            Doc.Examples examples <- pure interface.documentation
            assertEq t (examples <&> (.result)) [["y", "z"]]

          it t "blank line" $ do
            interface <- scrod t ["-- | >>> x", "-- <BLANKLINE>", "module M where"]
            Doc.Examples examples <- pure interface.documentation
            assertEq t (examples <&> (.result)) [[""]]

      describe t "header" $ do
        it t "title" $ do
          interface <- scrod t ["-- | = x", "module M where"]
          Doc.Header header <- pure interface.documentation
          assertEq t header.title $ Doc.String "x"

        describe t "level" $ do
          it t "one" $ do
            interface <- scrod t ["-- | = x", "module M where"]
            Doc.Header header <- pure interface.documentation
            assertEq t header.level Level.One

          it t "two" $ do
            interface <- scrod t ["-- | == x", "module M where"]
            Doc.Header header <- pure interface.documentation
            assertEq t header.level Level.Two

          it t "three" $ do
            interface <- scrod t ["-- | === x", "module M where"]
            Doc.Header header <- pure interface.documentation
            assertEq t header.level Level.Three

          it t "four" $ do
            interface <- scrod t ["-- | ==== x", "module M where"]
            Doc.Header header <- pure interface.documentation
            assertEq t header.level Level.Four

          it t "five" $ do
            interface <- scrod t ["-- | ===== x", "module M where"]
            Doc.Header header <- pure interface.documentation
            assertEq t header.level Level.Five

          it t "six" $ do
            interface <- scrod t ["-- | ====== x", "module M where"]
            Doc.Header header <- pure interface.documentation
            assertEq t header.level Level.Six

      describe t "table" $ do
        it t "works" $ do
          interface <-
            scrod
              t
              [ "-- |",
                "-- +---+---+",
                "-- | a | b |",
                "-- +---+---+",
                "module M where"
              ]
          assertEq t interface.documentation $
            Doc.Table
              Table.empty
                { Table.bodyRows =
                    [ [ TableCell.empty $ Doc.String "a",
                        TableCell.empty $ Doc.String "b"
                      ]
                    ]
                }

        it t "with header" $ do
          interface <-
            scrod
              t
              [ "-- |",
                "-- +---+---+",
                "-- | a | b |",
                "-- +===+===+",
                "-- | c | d |",
                "-- +---+---+",
                "module M where"
              ]
          assertEq t interface.documentation $
            Doc.Table
              Table.empty
                { Table.headerRows =
                    [ [ TableCell.empty $ Doc.String "a",
                        TableCell.empty $ Doc.String "b"
                      ]
                    ],
                  Table.bodyRows =
                    [ [ TableCell.empty $ Doc.String "c",
                        TableCell.empty $ Doc.String "d"
                      ]
                    ]
                }

        it t "with colspan" $ do
          interface <-
            scrod
              t
              [ "-- |",
                "-- +---+---+",
                "-- | a | b |",
                "-- +---+---+",
                "-- | c     |",
                "-- +---+---+",
                "module M where"
              ]
          assertEq t interface.documentation $
            Doc.Table
              Table.empty
                { Table.bodyRows =
                    [ [ TableCell.empty $ Doc.String "a",
                        TableCell.empty $ Doc.String "b"
                      ],
                      [ (TableCell.empty $ Doc.String "c") {TableCell.colspan = 2}
                      ]
                    ]
                }

        it t "with rowspan" $ do
          interface <-
            scrod
              t
              [ "-- |",
                "-- +---+---+",
                "-- | a | b |",
                "-- +   +---+",
                "-- |   | c |",
                "-- +---+---+",
                "module M where"
              ]
          assertEq t interface.documentation $
            Doc.Table
              Table.empty
                { Table.bodyRows =
                    [ [ (TableCell.empty $ Doc.String "a\n\n") {TableCell.rowspan = 2},
                        TableCell.empty $ Doc.String "b"
                      ],
                      [ TableCell.empty $ Doc.String "c"
                      ]
                    ]
                }

  describe t "since" $ do
    it t "is empty by default" $ do
      interface <- scrod t []
      assertEq t interface.since Nothing

    describe t "package" $ do
      it t "works" $ do
        -- The code is in place to populate this field, but Haddock itself does
        -- not handle it. So if we update Haddock and this test starts failing,
        -- simply update the test.
        --
        -- > assertEq t (interface.since.package <&> (.value)) $ Just "p"
        interface <- scrod t ["-- | @since p-0", "module M where"]
        assertEq t interface.documentation $ Doc.Paragraph (Doc.String "@since p-0")

    describe t "version" $ do
      it t "parses a simple version" $ do
        interface <- scrod t ["-- | @since 0", "module M where"]
        assertEq t (interface.since <&> (.version.value)) . Just $ NonEmpty.fromList [0]

      it t "parses a complex version" $ do
        interface <- scrod t ["-- | @since 1.2", "module M where"]
        assertEq t (interface.since <&> (.version.value)) . Just $ NonEmpty.fromList [1, 2]

  describe t "name" $ do
    it t "has no module name by default" $ do
      interface <- scrod t []
      assertEq t interface.name Nothing

    it t "gets a simple name" $ do
      interface <- scrod t ["module M where"]
      assertEq t (interface.name <&> (.value.value)) $ Just "M"

    it t "gets a complex name" $ do
      interface <- scrod t ["module M.N where"]
      assertEq t (interface.name <&> (.value.value)) $ Just "M.N"

    it t "gets the line" $ do
      interface <- scrod t ["module M where"]
      assertEq t (interface.name <&> (.location.line.value)) $ Just 1

    it t "gets the column" $ do
      interface <- scrod t ["module M where"]
      assertEq t (interface.name <&> (.location.column.value)) $ Just 8

  describe t "warning" $ do
    it t "has no warning by default" $ do
      interface <- scrod t []
      assertEq t interface.warning Nothing

    describe t "category" $ do
      it t "uses deprecations for warning" $ do
        interface <- scrod t ["module M {-# warning \"x\" #-} where"]
        assertEq t (interface.warning <&> (.category.value)) $ Just "deprecations"

      it t "uses deprecations for deprecated" $ do
        interface <- scrod t ["module M {-# deprecated \"x\" #-} where"]
        assertEq t (interface.warning <&> (.category.value)) $ Just "deprecations"

      it t "works with x-custom" $ do
        interface <- scrod t ["module M {-# warning in \"x-custom\" \"y\" #-} where"]
        assertEq t (interface.warning <&> (.category.value)) $ Just "x-custom"

    describe t "value" $ do
      it t "works with a string" $ do
        interface <- scrod t ["module M {-# warning \"x\" #-} where"]
        assertEq t (interface.warning <&> (.value)) $ Just "x"

      it t "works with an empty list" $ do
        interface <- scrod t ["module M {-# warning [] #-} where"]
        assertEq t (interface.warning <&> (.value)) $ Just ""

      it t "works with a singleton list" $ do
        interface <- scrod t ["module M {-# warning [\"x\"] #-} where"]
        assertEq t (interface.warning <&> (.value)) $ Just "x"

      it t "works with a list" $ do
        interface <- scrod t ["module M {-# warning [\"x\", \"y\"] #-} where"]
        assertEq t (interface.warning <&> (.value)) $ Just "x\ny"

  describe t "exports" $ do
    it t "has no exports by default" $ do
      interface <- scrod t []
      assertEq t interface.exports Nothing

    it t "has no exports without module declaration" $ do
      interface <- scrod t ["x = 1"]
      assertEq t interface.exports Nothing

    it t "has no exports without explicit list" $ do
      interface <- scrod t ["module M where"]
      assertEq t interface.exports Nothing

    it t "handles empty export list" $ do
      interface <- scrod t ["module M () where"]
      assertEq t interface.exports $ Just []

    describe t "var" $ do
      it t "exports a variable" $ do
        interface <- scrod t ["module M (x) where"]
        Just [Export.Identifier exportIdentifier] <- pure interface.exports
        assertEq t exportIdentifier.name.name "x"

      it t "exports an operator" $ do
        interface <- scrod t ["module M ((<>)) where"]
        Just [Export.Identifier exportIdentifier] <- pure interface.exports
        assertEq t exportIdentifier.name.name "<>"

      it t "exports multiple variables" $ do
        interface <- scrod t ["module M (x, y) where"]
        Just [Export.Identifier x, Export.Identifier y] <- pure interface.exports
        assertEq t x.name.name "x"
        assertEq t y.name.name "y"

    describe t "thing" $ do
      it t "exports a type without subordinates" $ do
        interface <- scrod t ["module M (T) where"]
        Just [Export.Identifier exportIdentifier] <- pure interface.exports
        assertEq t exportIdentifier.name.name "T"
        assertEq t exportIdentifier.subordinates Nothing

      it t "exports a type with wildcard" $ do
        interface <- scrod t ["module M (T(..)) where"]
        Just [Export.Identifier exportIdentifier] <- pure interface.exports
        Just subordinates <- pure exportIdentifier.subordinates
        assertEq t subordinates.wildcard True
        assertEq t subordinates.explicit []

      it t "exports a type with explicit children" $ do
        interface <- scrod t ["module M (T(A, B)) where"]
        Just [Export.Identifier exportIdentifier] <- pure interface.exports
        Just subordinates <- pure exportIdentifier.subordinates
        assertEq t subordinates.wildcard False
        assertEq t (subordinates.explicit <&> (.name)) ["A", "B"]

      it t "exports a type with wildcard and explicit children" $ do
        interface <- scrod t ["{-# language PatternSynonyms #-}", "module M (T(.., P)) where"]
        Just [Export.Identifier exportIdentifier] <- pure interface.exports
        Just subordinates <- pure exportIdentifier.subordinates
        assertEq t subordinates.wildcard True
        assertEq t (subordinates.explicit <&> (.name)) ["P"]

    describe t "module" $ do
      it t "re-exports a module" $ do
        interface <- scrod t ["module M (module X) where"]
        Just [Export.Identifier exportIdentifier] <- pure interface.exports
        assertEq t exportIdentifier.name.name "X"
        assertEq t exportIdentifier.name.kind $ Just ExportNameKind.Module

      it t "re-exports a qualified module" $ do
        interface <- scrod t ["module M (module Data.List) where"]
        Just [Export.Identifier exportIdentifier] <- pure interface.exports
        assertEq t exportIdentifier.name.name "Data.List"
        assertEq t exportIdentifier.name.kind $ Just ExportNameKind.Module

    describe t "namespace" $ do
      it t "exports with pattern namespace" $ do
        interface <- scrod t ["{-# language PatternSynonyms #-}", "module M (pattern P) where"]
        Just [Export.Identifier exportIdentifier] <- pure interface.exports
        assertEq t exportIdentifier.name.name "P"
        assertEq t exportIdentifier.name.kind $ Just ExportNameKind.Pattern

      it t "exports with type namespace" $ do
        interface <- scrod t ["module M (type T) where"]
        Just [Export.Identifier exportIdentifier] <- pure interface.exports
        assertEq t exportIdentifier.name.name "T"
        assertEq t exportIdentifier.name.kind $ Just ExportNameKind.Type

    describe t "warning" $ do
      it t "attaches warning to var" $ do
        interface <- scrod t ["module M ({-# WARNING \"wrn\" #-} x) where"]
        Just [Export.Identifier exportIdentifier] <- pure interface.exports
        Just warning <- pure exportIdentifier.warning
        assertEq t warning.category.value "deprecations"
        assertEq t warning.value "wrn"

      it t "attaches warning to thing" $ do
        interface <- scrod t ["module M ({-# WARNING \"wrn\" #-} T) where"]
        Just [Export.Identifier exportIdentifier] <- pure interface.exports
        Just warning <- pure exportIdentifier.warning
        assertEq t warning.category.value "deprecations"
        assertEq t warning.value "wrn"

      it t "attaches warning to thing with subordinates" $ do
        interface <- scrod t ["module M ({-# WARNING \"wrn\" #-} T(..)) where"]
        Just [Export.Identifier exportIdentifier] <- pure interface.exports
        Just warning <- pure exportIdentifier.warning
        assertEq t warning.value "wrn"

      it t "attaches deprecated to var" $ do
        interface <- scrod t ["module M ({-# DEPRECATED \"dep\" #-} x) where"]
        Just [Export.Identifier exportIdentifier] <- pure interface.exports
        Just warning <- pure exportIdentifier.warning
        assertEq t warning.category.value "deprecations"
        assertEq t warning.value "dep"

      it t "attaches warning with custom category" $ do
        interface <- scrod t ["module M ({-# WARNING in \"x-custom\" \"wrn\" #-} x) where"]
        Just [Export.Identifier exportIdentifier] <- pure interface.exports
        Just warning <- pure exportIdentifier.warning
        assertEq t warning.category.value "x-custom"
        assertEq t warning.value "wrn"

      it t "attaches warning to module re-export" $ do
        interface <- scrod t ["module M ({-# WARNING \"wrn\" #-} module X) where"]
        Just [Export.Identifier exportIdentifier] <- pure interface.exports
        Just warning <- pure exportIdentifier.warning
        assertEq t warning.value "wrn"

    describe t "documentation" $ do
      it t "handles section heading" $ do
        interface <- scrod t ["module M ( -- * Section", ") where"]
        assertEq t interface.exports $
          Just
            [ Export.Group
                Section.MkSection
                  { Section.header =
                      Header.MkHeader
                        { Header.level = Level.One,
                          Header.title = Doc.Paragraph (Doc.String "Section")
                        }
                  }
            ]

      it t "handles section heading level two" $ do
        interface <- scrod t ["module M ( -- ** Section", ") where"]
        assertEq t interface.exports $
          Just
            [ Export.Group
                Section.MkSection
                  { Section.header =
                      Header.MkHeader
                        { Header.level = Level.Two,
                          Header.title = Doc.Paragraph (Doc.String "Section")
                        }
                  }
            ]

      it t "handles inline doc" $ do
        interface <- scrod t ["module M ( -- | Some doc", ") where"]
        assertEq t interface.exports $ Just [Export.Doc . Doc.Paragraph $ Doc.String "Some doc"]

      it t "handles doc before export" $ do
        interface <- scrod t ["module M ( -- | foo", " bar ) where"]
        Just [Export.Doc _, Export.Identifier exportIdentifier] <- pure interface.exports
        assertEq t exportIdentifier.name.name "bar"

      it t "handles named doc reference" $ do
        interface <- scrod t ["module M ( -- $chunkName", ") where"]
        assertEq t interface.exports $ Just [Export.DocNamed "chunkName"]

      it t "handles doc attached to export" $ do
        interface <- scrod t ["module M ( x -- ^ y", ") where"]
        Just [Export.Identifier exportIdentifier] <- pure interface.exports
        assertEq t exportIdentifier.name.name "x"
        assertEq t exportIdentifier.doc . Just . Doc.Paragraph $ Doc.String "y"

  describe t "items" $ do
    let itemAt l c =
          Located.MkLocated
            { Located.location =
                Location.MkLocation
                  { Location.line = Line.MkLine l,
                    Location.column = Column.MkColumn c
                  },
              Located.value = Item.MkItem
            }

    describe t "TyClD" $ do
      describe t "FamDecl" $ do
        it t "type family open" $ do
          interface <- scrod t ["type family F a"]
          assertEq t interface.items [itemAt 1 1]

        it t "type family open multiple params" $ do
          interface <- scrod t ["type family F a b"]
          assertEq t interface.items [itemAt 1 1]

        it t "type family closed" $ do
          interface <- scrod t ["type family G a where", " G Int = Bool", " G a = Char"]
          assertEq t interface.items [itemAt 1 1]

        it t "type family closed empty" $ do
          interface <- scrod t ["type family H a where {}"]
          assertEq t interface.items [itemAt 1 1]

        it t "type family with kind sig" $ do
          interface <- scrod t ["type family I a :: Type"]
          assertEq t interface.items [itemAt 1 1]

        it t "type family with result kind variable" $ do
          interface <- scrod t ["type family I2 a = (r :: Type)"]
          assertEq t interface.items [itemAt 1 1]

        it t "type family with injectivity" $ do
          interface <- scrod t ["type family J a = r | r -> a"]
          assertEq t interface.items [itemAt 1 1]

        it t "data family" $ do
          interface <- scrod t ["data family D a"]
          assertEq t interface.items [itemAt 1 1]

        it t "data family with kind sig" $ do
          interface <- scrod t ["data family E a :: Type"]
          assertEq t interface.items [itemAt 1 1]

      describe t "SynDecl" $ do
        it t "basic" $ do
          interface <- scrod t ["type S = Int"]
          assertEq t interface.items [itemAt 1 1]

        it t "with params" $ do
          interface <- scrod t ["type T a = [a]"]
          assertEq t interface.items [itemAt 1 1]

        it t "with multiple params" $ do
          interface <- scrod t ["type U a b = Either a b"]
          assertEq t interface.items [itemAt 1 1]

        it t "with kind annotation" $ do
          interface <- scrod t ["type V (a :: Type) = Maybe a"]
          assertEq t interface.items [itemAt 1 1]

        it t "with forall" $ do
          interface <- scrod t ["type R = forall a. a -> a"]
          assertEq t interface.items [itemAt 1 1]

        it t "type operator" $ do
          interface <- scrod t ["type a + b = Either a b"]
          assertEq t interface.items [itemAt 1 1]

      describe t "DataDecl" $ do
        it t "data basic" $ do
          interface <- scrod t ["data A"]
          assertEq t interface.items [itemAt 1 1]

        it t "data with constructor" $ do
          interface <- scrod t ["data B = B"]
          assertEq t interface.items [itemAt 1 1, itemAt 1 10]

        it t "data with multiple constructors" $ do
          interface <- scrod t ["data C = C1 | C2 | C3"]
          assertEq t interface.items [itemAt 1 1, itemAt 1 10, itemAt 1 15, itemAt 1 20]

        it t "data with fields" $ do
          interface <- scrod t ["data D = D Int Bool"]
          assertEq t interface.items [itemAt 1 1, itemAt 1 10]

        it t "data with record" $ do
          interface <- scrod t ["data E = E { eInt :: Int, eBool :: Bool }"]
          assertEq t interface.items [itemAt 1 1, itemAt 1 10, itemAt 1 14, itemAt 1 27]

        it t "data with deriving" $ do
          interface <- scrod t ["data F = F deriving Show"]
          assertEq t interface.items [itemAt 1 1, itemAt 1 10]

        it t "data with deriving multiple" $ do
          interface <- scrod t ["data G = G deriving (Show, Eq)"]
          assertEq t interface.items [itemAt 1 1, itemAt 1 10]

        it t "data with deriving strategies" $ do
          interface <- scrod t ["data H = H deriving stock Show"]
          assertEq t interface.items [itemAt 1 1, itemAt 1 10]

        it t "data with deriving via" $ do
          interface <- scrod t ["newtype I = I Int deriving Show via Int"]
          assertEq t interface.items [itemAt 1 1, itemAt 1 13]

        it t "data with deriving anyclass" $ do
          interface <- scrod t ["data J = J deriving anyclass C"]
          assertEq t interface.items [itemAt 1 1, itemAt 1 10]

        it t "data with deriving newtype" $ do
          interface <- scrod t ["newtype K = K Int deriving newtype Num"]
          assertEq t interface.items [itemAt 1 1, itemAt 1 13]

        it t "data with type params" $ do
          interface <- scrod t ["data L a = L a"]
          assertEq t interface.items [itemAt 1 1, itemAt 1 12]

        it t "data with phantom type" $ do
          interface <- scrod t ["data M a = M"]
          assertEq t interface.items [itemAt 1 1, itemAt 1 12]

        it t "data with kind sig" $ do
          interface <- scrod t ["data N (a :: Type) = N"]
          assertEq t interface.items [itemAt 1 1, itemAt 1 22]

        it t "data with existential" $ do
          interface <- scrod t ["data O = forall a . Show a => O a"]
          assertEq t interface.items [itemAt 1 1, itemAt 1 10]

        it t "data with GADT" $ do
          interface <- scrod t ["data P a where", " P1 :: P Int", " P2 :: P Bool"]
          assertEq t interface.items [itemAt 1 1, itemAt 2 2, itemAt 3 2]

        it t "data with GADT record" $ do
          interface <- scrod t ["data Q a where Q :: { qVal :: Int } -> Q Int"]
          assertEq t interface.items [itemAt 1 1, itemAt 1 16, itemAt 1 23]

        it t "data with strictness" $ do
          interface <- scrod t ["data R = R !Int"]
          assertEq t interface.items [itemAt 1 1, itemAt 1 10]

        it t "data with laziness" $ do
          interface <- scrod t ["data S = S ~Int"]
          assertEq t interface.items [itemAt 1 1, itemAt 1 10]

        it t "data with unpack" $ do
          interface <- scrod t ["data T = T {-# UNPACK #-} !Int"]
          assertEq t interface.items [itemAt 1 1, itemAt 1 10]

        it t "data with nounpack" $ do
          interface <- scrod t ["data U = U {-# NOUNPACK #-} !Int"]
          assertEq t interface.items [itemAt 1 1, itemAt 1 10]

        it t "newtype basic" $ do
          interface <- scrod t ["newtype V = V Int"]
          assertEq t interface.items [itemAt 1 1, itemAt 1 13]

        it t "newtype with record" $ do
          interface <- scrod t ["newtype W = W { unW :: Int }"]
          assertEq t interface.items [itemAt 1 1, itemAt 1 13, itemAt 1 17]

        it t "data with context" $ do
          interface <- scrod t ["data Eq a => X a = X a"]
          assertEq t interface.items [itemAt 1 1, itemAt 1 20]

        it t "data with forall" $ do
          interface <- scrod t ["data Y = forall a . Y a"]
          assertEq t interface.items [itemAt 1 1, itemAt 1 10]

        it t "data with linear field" $ do
          interface <- scrod t ["data Z a b = Z (a %1 -> b)"]
          assertEq t interface.items [itemAt 1 1, itemAt 1 14]

        it t "data with type operator name" $ do
          interface <- scrod t ["data a :+: b = L a | R b"]
          assertEq t interface.items [itemAt 1 1, itemAt 1 16, itemAt 1 22]

        it t "data with multiple deriving clauses" $ do
          interface <- scrod t ["data AA = AA deriving Show deriving Eq"]
          assertEq t interface.items [itemAt 1 1, itemAt 1 11]

        it t "type data" $ do
          interface <- scrod t ["type data TBool = TTrue | TFalse"]
          assertEq t interface.items [itemAt 1 1, itemAt 1 19, itemAt 1 27]

      describe t "ClassDecl" $ do
        it t "basic" $ do
          interface <- scrod t ["class Cls a"]
          assertEq t interface.items [itemAt 1 1]

        it t "with method" $ do
          interface <- scrod t ["class Cls a where method :: a -> a"]
          assertEq t interface.items [itemAt 1 1]

        it t "with default method" $ do
          interface <- scrod t ["class Cls a where", " method :: a -> a", " method = id"]
          assertEq t interface.items [itemAt 1 1]

        it t "with multiple methods" $ do
          interface <- scrod t ["class Cls a where", " m1 :: a", " m2 :: a -> a"]
          assertEq t interface.items [itemAt 1 1]

        it t "with superclass" $ do
          interface <- scrod t ["class Eq a => Cls a"]
          assertEq t interface.items [itemAt 1 1]

        it t "with multiple superclasses" $ do
          interface <- scrod t ["class (Eq a, Ord a) => Cls a"]
          assertEq t interface.items [itemAt 1 1]

        it t "with fundeps" $ do
          interface <- scrod t ["class Cls a b | a -> b"]
          assertEq t interface.items [itemAt 1 1]

        it t "with fundeps multiple" $ do
          interface <- scrod t ["class Cls a b c | a -> b, b -> c"]
          assertEq t interface.items [itemAt 1 1]

        it t "with associated type" $ do
          interface <- scrod t ["class Cls a where type T a"]
          assertEq t interface.items [itemAt 1 1]

        it t "with associated type with kind sig" $ do
          interface <- scrod t ["class Cls a where type T a :: Type -> Type"]
          assertEq t interface.items [itemAt 1 1]

        it t "with associated type default" $ do
          interface <- scrod t ["class Cls a where", " type T a", " type T a = Int"]
          assertEq t interface.items [itemAt 1 1]

        it t "with associated data" $ do
          interface <- scrod t ["class Cls a where data D a"]
          assertEq t interface.items [itemAt 1 1]

        it t "with associated data with kind sig" $ do
          interface <- scrod t ["class Cls a where data D a :: Type"]
          assertEq t interface.items [itemAt 1 1]

        it t "with quantified constraint" $ do
          interface <- scrod t ["class (forall x. Eq (f x)) => Cls f"]
          assertEq t interface.items [itemAt 1 1]

        it t "with default signature" $ do
          interface <- scrod t ["class Cls a where", " method :: a -> a", " default method :: Show a => a -> a"]
          assertEq t interface.items [itemAt 1 1]

        it t "with minimal pragma" $ do
          interface <- scrod t ["class Cls a where", " m1 :: a", " m2 :: a", " {-# MINIMAL m1 | m2 #-}"]
          assertEq t interface.items [itemAt 1 1]

        it t "with kind sig" $ do
          interface <- scrod t ["class Cls (a :: Type)"]
          assertEq t interface.items [itemAt 1 1]

        it t "no params" $ do
          interface <- scrod t ["class Cls"]
          assertEq t interface.items [itemAt 1 1]

        it t "multi-param" $ do
          interface <- scrod t ["class Cls a b"]
          assertEq t interface.items [itemAt 1 1]

    describe t "InstD" $ do
      describe t "ClsInstD" $ do
        it t "no params" $ do
          interface <- scrod t ["instance Cls where"]
          assertEq t interface.items [itemAt 1 1]

        it t "basic" $ do
          interface <- scrod t ["instance Cls Int where"]
          assertEq t interface.items [itemAt 1 1]

        it t "multi params" $ do
          interface <- scrod t ["instance Cls Int Bool where"]
          assertEq t interface.items [itemAt 1 1]

        it t "with method" $ do
          interface <- scrod t ["instance Cls Int where method = id"]
          assertEq t interface.items [itemAt 1 1]

        it t "with type application" $ do
          interface <- scrod t ["instance Cls [a] where"]
          assertEq t interface.items [itemAt 1 1]

        it t "with context" $ do
          interface <- scrod t ["instance Eq a => Cls [a] where"]
          assertEq t interface.items [itemAt 1 1]

        it t "with multiple contexts" $ do
          interface <- scrod t ["instance (Eq a, Ord a) => Cls [a] where"]
          assertEq t interface.items [itemAt 1 1]

        it t "overlapping" $ do
          interface <- scrod t ["instance {-# OVERLAPPING #-} Cls [Int] where"]
          assertEq t interface.items [itemAt 1 1]

        it t "overlappable" $ do
          interface <- scrod t ["instance {-# OVERLAPPABLE #-} Cls [a] where"]
          assertEq t interface.items [itemAt 1 1]

        it t "overlaps" $ do
          interface <- scrod t ["instance {-# OVERLAPS #-} Cls [Int] where"]
          assertEq t interface.items [itemAt 1 1]

        it t "incoherent" $ do
          interface <- scrod t ["instance {-# INCOHERENT #-} Cls [Int] where"]
          assertEq t interface.items [itemAt 1 1]

        it t "with explicit forall" $ do
          interface <- scrod t ["instance forall a. Cls [a] where"]
          assertEq t interface.items [itemAt 1 1]

        it t "with type application" $ do
          interface <- scrod t ["instance Cls @Type Int where"]
          assertEq t interface.items [itemAt 1 1]

      describe t "DataFamInstD" $ do
        it t "basic" $ do
          interface <- scrod t ["data family D a", "data instance D Int = DInt"]
          assertEq t interface.items [itemAt 1 1, itemAt 2 1]

        it t "with constructor" $ do
          interface <- scrod t ["data family D a", "data instance D Bool = DBool Bool"]
          assertEq t interface.items [itemAt 1 1, itemAt 2 1]

        it t "with multiple constructors" $ do
          interface <- scrod t ["data family D a", "data instance D Char = DC1 | DC2 Char"]
          assertEq t interface.items [itemAt 1 1, itemAt 2 1]

        it t "with record" $ do
          interface <- scrod t ["data family D a", "data instance D () = DUnit { dUnit :: () }"]
          assertEq t interface.items [itemAt 1 1, itemAt 2 1]

        it t "newtype instance" $ do
          interface <- scrod t ["data family D a", "newtype instance D Float = DFloat Float"]
          assertEq t interface.items [itemAt 1 1, itemAt 2 1]

        it t "with GADT" $ do
          interface <- scrod t ["data family D a", "data instance D (Maybe a) where DMaybe :: a -> D (Maybe a)"]
          assertEq t interface.items [itemAt 1 1, itemAt 2 1]

        it t "with forall" $ do
          interface <- scrod t ["data family D a", "data instance forall a . D [a] = DList [a]"]
          assertEq t interface.items [itemAt 1 1, itemAt 2 1]

        it t "with deriving" $ do
          interface <- scrod t ["data family D a", "data instance D Word = DWord Word deriving Show"]
          assertEq t interface.items [itemAt 1 1, itemAt 2 1]

      describe t "TyFamInstD" $ do
        it t "basic" $ do
          interface <- scrod t ["type family F a", "type instance F Int = Bool"]
          assertEq t interface.items [itemAt 1 1, itemAt 2 1]

        it t "with type var" $ do
          interface <- scrod t ["type family F a", "type instance F [a] = a"]
          assertEq t interface.items [itemAt 1 1, itemAt 2 1]

        it t "with nested" $ do
          interface <- scrod t ["type family F a", "type instance F (Maybe a) = Either () a"]
          assertEq t interface.items [itemAt 1 1, itemAt 2 1]

        it t "with forall" $ do
          interface <- scrod t ["type family F a", "type instance forall a. F [a] = a"]
          assertEq t interface.items [itemAt 1 1, itemAt 2 1]

    describe t "DerivD" $ do
      describe t "DerivDecl" $ do
        it t "basic" $ do
          interface <- scrod t ["data A = A", "deriving instance Show A"]
          assertEq t interface.items [itemAt 1 1, itemAt 1 10, itemAt 2 1]

        it t "with context" $ do
          interface <- scrod t ["data B a = B a", "deriving instance Show a => Show (B a)"]
          assertEq t interface.items [itemAt 1 1, itemAt 1 12, itemAt 2 1]

        it t "stock" $ do
          interface <- scrod t ["data C = C", "deriving stock instance Show C"]
          assertEq t interface.items [itemAt 1 1, itemAt 1 10, itemAt 2 1]

        it t "newtype" $ do
          interface <- scrod t ["newtype D = D Int", "deriving newtype instance Num D"]
          assertEq t interface.items [itemAt 1 1, itemAt 1 13, itemAt 2 1]

        it t "anyclass" $ do
          interface <- scrod t ["class Cls a", "data E = E", "deriving anyclass instance Cls E"]
          assertEq t interface.items [itemAt 1 1, itemAt 2 1, itemAt 2 10, itemAt 3 1]

        it t "via" $ do
          interface <- scrod t ["newtype F = F Int", "deriving via Int instance Show F"]
          assertEq t interface.items [itemAt 1 1, itemAt 1 13, itemAt 2 1]

    describe t "ValD" $ do
      describe t "FunBind" $ do
        it t "basic" $ do
          interface <- scrod t ["f x = x"]
          assertEq t interface.items [itemAt 1 1]

        it t "with multiple args" $ do
          interface <- scrod t ["f x y z = x"]
          assertEq t interface.items [itemAt 1 1]

        it t "with pattern matching" $ do
          interface <- scrod t ["f [] = 0", "f (x:xs) = 1 + f xs"]
          assertEq t interface.items [itemAt 1 1]

        it t "with guards" $ do
          interface <- scrod t ["f x | x > 0 = 1 | otherwise = 0"]
          assertEq t interface.items [itemAt 1 1]

        it t "with where" $ do
          interface <- scrod t ["f x = y where y = x + 1"]
          assertEq t interface.items [itemAt 1 1]

        it t "with let" $ do
          interface <- scrod t ["f x = let y = x + 1 in y"]
          assertEq t interface.items [itemAt 1 1]

        it t "operator" $ do
          interface <- scrod t ["x +++ y = x + y"]
          assertEq t interface.items [itemAt 1 1]

        it t "operator prefix" $ do
          interface <- scrod t ["(+++) x y = x + y"]
          assertEq t interface.items [itemAt 1 1]

        it t "infix" $ do
          interface <- scrod t [" x `plus` y = x + y "]
          assertEq t interface.items [itemAt 1 2]

        it t "with view pattern" $ do
          interface <- scrod t ["f (show -> s) = s"]
          assertEq t interface.items [itemAt 1 1]

        it t "with pattern guard" $ do
          interface <- scrod t ["f x | Just y <- g x = y | otherwise = x"]
          assertEq t interface.items [itemAt 1 1]

        it t "with type abstraction" $ do
          interface <- scrod t ["f @a (x :: a) = x"]
          assertEq t interface.items [itemAt 1 1]

      describe t "PatBind" $ do
        it t "simple" $ do
          interface <- scrod t ["x = 1"]
          assertEq t interface.items [itemAt 1 1]

        it t "tuple" $ do
          interface <- scrod t ["(a, b) = (1, 2)"]
          assertEq t interface.items [itemAt 1 1]

        it t "list" $ do
          interface <- scrod t ["[x, y] = [1, 2]"]
          assertEq t interface.items [itemAt 1 1]

        it t "cons" $ do
          interface <- scrod t ["(h:t) = [1, 2, 3]"]
          assertEq t interface.items [itemAt 1 1]

        it t "record" $ do
          interface <- scrod t ["data T = C { f :: Int }", "C { f = x } = C 1"]
          assertEq t interface.items [itemAt 1 1, itemAt 1 10, itemAt 1 14, itemAt 2 1]

        it t "as pattern" $ do
          interface <- scrod t ["all@(x:xs) = [1, 2, 3]"]
          assertEq t interface.items [itemAt 1 1]

        it t "wildcard" $ do
          interface <- scrod t ["_ = undefined"]
          assertEq t interface.items [itemAt 1 1]

        it t "lazy" $ do
          interface <- scrod t ["~(a, b) = undefined"]
          assertEq t interface.items [itemAt 1 1]

        it t "bang" $ do
          interface <- scrod t ["!x = 1"]
          assertEq t interface.items [itemAt 1 1]

        it t "with type signature" $ do
          interface <- scrod t ["(x :: Int) = 1"]
          assertEq t interface.items [itemAt 1 1]

        it t "with view pattern" $ do
          interface <- scrod t ["(reverse -> xs) = [1,2,3]"]
          assertEq t interface.items [itemAt 1 1]

      describe t "PatSynBind" $ do
        it t "unidirectional" $ do
          interface <- scrod t ["{-# language PatternSynonyms #-} pattern P x = Just x"]
          assertEq t interface.items [itemAt 1 34]

        it t "bidirectional implicit" $ do
          interface <- scrod t ["{-# language PatternSynonyms #-} pattern Q x = (x, x)"]
          assertEq t interface.items [itemAt 1 34]

        it t "bidirectional explicit" $ do
          interface <- scrod t ["{-# language PatternSynonyms #-} pattern R x <- Just x where R x = Just x"]
          assertEq t interface.items [itemAt 1 34]

        it t "view pattern" $ do
          interface <- scrod t ["{-# language PatternSynonyms #-} pattern S x <- (show -> x)"]
          assertEq t interface.items [itemAt 1 34]

        it t "record" $ do
          interface <- scrod t ["{-# language PatternSynonyms #-} pattern T { tField } = Just tField"]
          assertEq t interface.items [itemAt 1 34]

        it t "prefix" $ do
          interface <- scrod t ["{-# language PatternSynonyms #-} pattern P x = Just x"]
          assertEq t interface.items [itemAt 1 34]

        it t "infix" $ do
          interface <- scrod t ["{-# language PatternSynonyms #-} pattern x :+: y = (x, y)"]
          assertEq t interface.items [itemAt 1 34]

    describe t "SigD" $ do
      describe t "TypeSig" $ do
        it t "basic" $ do
          interface <- scrod t ["f :: Int"]
          assertEq t interface.items [itemAt 1 1]

        it t "with args" $ do
          interface <- scrod t ["f :: Int -> Bool"]
          assertEq t interface.items [itemAt 1 1]

        it t "with implicit forall" $ do
          interface <- scrod t ["f :: a -> a"]
          assertEq t interface.items [itemAt 1 1]

        it t "with explicit forall visible" $ do
          interface <- scrod t ["f :: forall a . a -> a"]
          assertEq t interface.items [itemAt 1 1]

        it t "with explicit forall invisible" $ do
          interface <- scrod t ["f :: forall {a} . a -> a"]
          assertEq t interface.items [itemAt 1 1]

        it t "with context" $ do
          interface <- scrod t ["f :: Eq a => a -> Bool"]
          assertEq t interface.items [itemAt 1 1]

        it t "with multiple names" $ do
          interface <- scrod t ["f, g :: Int"]
          assertEq t interface.items [itemAt 1 1]

        it t "with kind sig" $ do
          interface <- scrod t ["f :: forall (a :: Type) . a -> a"]
          assertEq t interface.items [itemAt 1 1]

        it t "with linear arrow" $ do
          interface <- scrod t ["f :: a %1 -> b"]
          assertEq t interface.items [itemAt 1 1]

        it t "with multiplicity poly" $ do
          interface <- scrod t ["f :: a %m -> b"]
          assertEq t interface.items [itemAt 1 1]

        it t "with visible forall" $ do
          interface <- scrod t ["{-# LANGUAGE RequiredTypeArguments #-} f :: forall a -> a -> a"]
          assertEq t interface.items [itemAt 1 40]

      describe t "PatSynSig" $ do
        it t "basic" $ do
          interface <- scrod t ["{-# LANGUAGE PatternSynonyms #-} pattern P :: Int -> Maybe Int"]
          assertEq t interface.items [itemAt 1 34]

        it t "with forall provided" $ do
          interface <- scrod t ["{-# LANGUAGE PatternSynonyms #-} pattern Q :: forall a . a -> Maybe a"]
          assertEq t interface.items [itemAt 1 34]

        it t "with forall required" $ do
          interface <- scrod t ["{-# LANGUAGE PatternSynonyms #-} pattern R :: forall a . Show a => a -> Maybe a"]
          assertEq t interface.items [itemAt 1 34]

        it t "bidirectional" $ do
          interface <- scrod t ["{-# LANGUAGE PatternSynonyms #-} pattern S :: forall a . () => (Eq a) => a -> Maybe a"]
          assertEq t interface.items [itemAt 1 34]

      describe t "ClassOpSig" $ do
        it t "basic" $ do
          interface <- scrod t ["class C a where op :: a -> a"]
          assertEq t interface.items [itemAt 1 1]

        it t "default" $ do
          interface <- scrod t ["class C a where", " op :: a -> a", " default op :: a -> a"]
          assertEq t interface.items [itemAt 1 1]

        it t "multiple" $ do
          interface <- scrod t ["class C a where op1, op2 :: a -> a"]
          assertEq t interface.items [itemAt 1 1]

      describe t "FixSig" $ do
        it t "infixl" $ do
          interface <- scrod t ["infixl 6 +++"]
          assertEq t interface.items [itemAt 1 1]

        it t "infixr" $ do
          interface <- scrod t ["infixr 5 +++"]
          assertEq t interface.items [itemAt 1 1]

        it t "infix" $ do
          interface <- scrod t ["infix 4 +++"]
          assertEq t interface.items [itemAt 1 1]

        it t "non-operator" $ do
          interface <- scrod t [" infix 5 `T` "]
          assertEq t interface.items [itemAt 1 2]

        it t "multiple" $ do
          interface <- scrod t ["infixl 6 #, %"]
          assertEq t interface.items [itemAt 1 1]

        it t "precedence 0" $ do
          interface <- scrod t ["infixl 0 +++"]
          assertEq t interface.items [itemAt 1 1]

        it t "precedence 9" $ do
          interface <- scrod t ["infixr 9 +++"]
          assertEq t interface.items [itemAt 1 1]

      describe t "InlineSig" $ do
        it t "inline" $ do
          interface <- scrod t ["{-# INLINE f #-}"]
          assertEq t interface.items [itemAt 1 1]

        it t "inline operator" $ do
          interface <- scrod t ["{-# INLINE (+++) #-}"]
          assertEq t interface.items [itemAt 1 1]

        it t "conlike" $ do
          interface <- scrod t ["{-# INLINE CONLIKE f #-}"]
          assertEq t interface.items [itemAt 1 1]

        it t "noinline" $ do
          interface <- scrod t ["{-# NOINLINE f #-}"]
          assertEq t interface.items [itemAt 1 1]

        it t "inlinable" $ do
          interface <- scrod t ["{-# INLINABLE f #-}"]
          assertEq t interface.items [itemAt 1 1]

        it t "inline phase" $ do
          interface <- scrod t ["{-# INLINE [2] f #-}"]
          assertEq t interface.items [itemAt 1 1]

        it t "inline before phase" $ do
          interface <- scrod t ["{-# INLINE [~2] f #-}"]
          assertEq t interface.items [itemAt 1 1]

        it t "opaque" $ do
          interface <- scrod t ["{-# OPAQUE f #-}"]
          assertEq t interface.items [itemAt 1 1]

      describe t "SpecSig" $ do
        it t "basic" $ do
          interface <- scrod t ["{-# SPECIALIZE f :: Int -> Int #-}"]
          assertEq t interface.items [itemAt 1 1]

        it t "uk" $ do
          interface <- scrod t ["{-# SPECIALISE f :: Int -> Int #-}"]
          assertEq t interface.items [itemAt 1 1]

        it t "with inline" $ do
          interface <- scrod t ["{-# SPECIALIZE INLINE f :: Int -> Int #-}"]
          assertEq t interface.items [itemAt 1 1]

        it t "with noinline" $ do
          interface <- scrod t ["{-# SPECIALIZE NOINLINE f :: Int -> Int #-}"]
          assertEq t interface.items [itemAt 1 1]

        it t "with phase" $ do
          interface <- scrod t ["{-# SPECIALIZE [1] f :: Int -> Int #-}"]
          assertEq t interface.items [itemAt 1 1]

        it t "with before phase" $ do
          interface <- scrod t ["{-# SPECIALIZE [~1] f :: Int -> Int #-}"]
          assertEq t interface.items [itemAt 1 1]

      describe t "SpecInstSig" $ do
        it t "basic" $ do
          interface <- scrod t ["instance Cls [a] where {-# SPECIALIZE instance Cls [Int] #-}"]
          assertEq t interface.items [itemAt 1 1]

        it t "with context" $ do
          interface <- scrod t ["{-# SPECIALIZE instance Eq a => Cls [a] #-}"]
          assertEq t interface.items [itemAt 1 1]

      describe t "MinimalSig" $ do
        it t "single" $ do
          interface <- scrod t ["class C a where", " m :: a", " {-# MINIMAL m #-}"]
          assertEq t interface.items [itemAt 1 1]

        it t "or" $ do
          interface <- scrod t ["class C a where", " m1, m2 :: a", " {-# MINIMAL m1 | m2 #-}"]
          assertEq t interface.items [itemAt 1 1]

        it t "and" $ do
          interface <- scrod t ["class C a where", " m1, m2 :: a", " {-# MINIMAL m1, m2 #-}"]
          assertEq t interface.items [itemAt 1 1]

        it t "nested" $ do
          interface <- scrod t ["class C a where", " m1, m2, m3 :: a", " {-# MINIMAL (m1, m2) | m3 #-}"]
          assertEq t interface.items [itemAt 1 1]

        it t "empty" $ do
          interface <- scrod t ["class C a where", " m :: a", " m = undefined", " {-# MINIMAL #-}"]
          assertEq t interface.items [itemAt 1 1]

      describe t "SCCFunSig" $ do
        it t "basic" $ do
          interface <- scrod t ["{-# SCC f #-}"]
          assertEq t interface.items [itemAt 1 1]

        it t "with string" $ do
          interface <- scrod t ["{-# SCC f \"cost-centre-name\" #-}"]
          assertEq t interface.items [itemAt 1 1]

      describe t "CompleteMatchSig" $ do
        it t "basic" $ do
          interface <- scrod t ["{-# COMPLETE P #-}"]
          assertEq t interface.items [itemAt 1 1]

        it t "multiple" $ do
          interface <- scrod t ["{-# COMPLETE P, Q #-}"]
          assertEq t interface.items [itemAt 1 1]

        it t "with type" $ do
          interface <- scrod t ["{-# COMPLETE P :: Maybe #-}"]
          assertEq t interface.items [itemAt 1 1]

        it t "with type multiple" $ do
          interface <- scrod t ["{-# COMPLETE P, Q :: Either #-}"]
          assertEq t interface.items [itemAt 1 1]

    describe t "KindSigD" $ do
      describe t "StandaloneKindSig" $ do
        it t "basic" $ do
          interface <- scrod t ["type T :: Type", "data T"]
          assertEq t interface.items [itemAt 1 1, itemAt 2 1]

        it t "with arrow" $ do
          interface <- scrod t ["type T :: Type -> Type", "data T a"]
          assertEq t interface.items [itemAt 1 1, itemAt 2 1]

        it t "with constraint" $ do
          interface <- scrod t ["type C :: Type -> Constraint", "class C a"]
          assertEq t interface.items [itemAt 1 1, itemAt 2 1]

        it t "with poly kind" $ do
          interface <- scrod t ["type T :: forall k . k -> Type", "data T a"]
          assertEq t interface.items [itemAt 1 1, itemAt 2 1]

        it t "for type family" $ do
          interface <- scrod t ["type F :: Type -> Type", "type family F a"]
          assertEq t interface.items [itemAt 1 1, itemAt 2 1]

        it t "for type synonym" $ do
          interface <- scrod t ["type S :: Type -> Type", "type S a = [a]"]
          assertEq t interface.items [itemAt 1 1, itemAt 2 1]

        it t "for data family" $ do
          interface <- scrod t ["type D :: Type -> Type", "data family D a"]
          assertEq t interface.items [itemAt 1 1, itemAt 2 1]

    describe t "DefD" $ do
      describe t "DefaultDecl" $ do
        it t "empty" $ do
          interface <- scrod t ["default ()"]
          assertEq t interface.items [itemAt 1 1]

        it t "basic" $ do
          interface <- scrod t ["default (Int)"]
          assertEq t interface.items [itemAt 1 1]

        it t "multiple" $ do
          interface <- scrod t ["default (Int, Double)"]
          assertEq t interface.items [itemAt 1 1]

    describe t "ForD" $ do
      describe t "ForeignImport" $ do
        it t "ccall" $ do
          interface <- scrod t ["foreign import ccall \"math.h sin\" c_sin :: Double -> Double"]
          assertEq t interface.items [itemAt 1 1]

        it t "ccall safe" $ do
          interface <- scrod t ["foreign import ccall safe \"sleep\" c_sleep :: Int -> IO Int"]
          assertEq t interface.items [itemAt 1 1]

        it t "ccall unsafe" $ do
          interface <- scrod t ["foreign import ccall unsafe \"getchar\" c_getchar :: IO Char"]
          assertEq t interface.items [itemAt 1 1]

        it t "capi" $ do
          interface <- scrod t ["{-# LANGUAGE CApiFFI #-} foreign import capi \"stdio.h getchar\" c_getchar :: IO Char"]
          assertEq t interface.items [itemAt 1 26]

        it t "stdcall (Windows)" $ do
          interface <- scrod t ["foreign import stdcall \"windows.h MessageBoxA\" c_msgbox :: Ptr () -> CString -> CString -> Int -> IO Int"]
          assertEq t interface.items [itemAt 1 1]

        it t "prim" $ do
          interface <- scrod t ["{-# LANGUAGE GHCForeignImportPrim, MagicHash #-} foreign import prim \"stg_foo\" foo# :: Int# -> Int#"]
          assertEq t interface.items [itemAt 1 50]

        it t "wrapper" $ do
          interface <- scrod t ["foreign import ccall \"wrapper\" mkCallback :: (Int -> IO Int) -> IO (FunPtr (Int -> IO Int))"]
          assertEq t interface.items [itemAt 1 1]

        it t "dynamic" $ do
          interface <- scrod t ["foreign import ccall \"dynamic\" callFunPtr :: FunPtr (Int -> IO Int) -> Int -> IO Int"]
          assertEq t interface.items [itemAt 1 1]

        it t "without string" $ do
          interface <- scrod t ["foreign import ccall sin :: Double -> Double"]
          assertEq t interface.items [itemAt 1 1]

      describe t "ForeignExport" $ do
        it t "ccall" $ do
          interface <- scrod t ["foreign export ccall foo :: Int -> Int"]
          assertEq t interface.items [itemAt 1 1]

        it t "ccall with name" $ do
          interface <- scrod t ["foreign export ccall \"hs_foo\" foo :: Int -> Int"]
          assertEq t interface.items [itemAt 1 1]

        it t "stdcall (Windows)" $ do
          interface <- scrod t ["foreign export stdcall foo :: Int -> IO Int"]
          assertEq t interface.items [itemAt 1 1]

    describe t "WarningD" $ do
      describe t "Warnings" $ do
        it t "one" $ do
          interface <- scrod t ["{-# WARNING x \"y\" #-}"]
          assertEq t interface.items [itemAt 1 1]

        it t "two" $ do
          interface <- scrod t ["{-# WARNING x, y \"z\" #-}"]
          assertEq t interface.items [itemAt 1 1]

        it t "empty list" $ do
          interface <- scrod t ["{-# WARNING x [] #-}"]
          assertEq t interface.items [itemAt 1 1]

        it t "singleton list" $ do
          interface <- scrod t ["{-# WARNING x [\"y\"] #-}"]
          assertEq t interface.items [itemAt 1 1]

        it t "list" $ do
          interface <- scrod t ["{-# WARNING x [\"y\", \"z\"] #-}"]
          assertEq t interface.items [itemAt 1 1]

        it t "deprecated" $ do
          interface <- scrod t ["{-# DEPRECATED x \"y\" #-}"]
          assertEq t interface.items [itemAt 1 1]

        it t "category" $ do
          interface <- scrod t ["{-# WARNING in \"x-foo\" bar \"qux\" #-}"]
          assertEq t interface.items [itemAt 1 1]

        it t "explicit data" $ do
          interface <- scrod t ["{-# WARNING data Foo \"bar\" #-}"]
          assertEq t interface.items [itemAt 1 1]

        it t "explicit type" $ do
          interface <- scrod t ["{-# WARNING type Foo \"bar\" #-}"]
          assertEq t interface.items [itemAt 1 1]

        it t "instance" $ do
          interface <- scrod t ["instance {-# WARNING \"x\" #-} Cls Typ"]
          assertEq t interface.items [itemAt 1 1]

        it t "deriving" $ do
          interface <- scrod t ["deriving instance {-# WARNING \"x\" #-} Cls Typ"]
          assertEq t interface.items [itemAt 1 1]

    describe t "AnnD" $ do
      describe t "HsAnnotation" $ do
        it t "value" $ do
          interface <- scrod t ["x = 0", "{-# ANN x () #-}"]
          assertEq t interface.items [itemAt 1 1, itemAt 2 1]

        it t "type" $ do
          interface <- scrod t ["{-# ANN type T () #-}"]
          assertEq t interface.items [itemAt 1 1]

        it t "module" $ do
          interface <- scrod t ["{-# ANN module () #-}"]
          assertEq t interface.items [itemAt 1 1]

    describe t "RuleD" $ do
      describe t "HsRules" $ do
        it t "basic" $ do
          interface <- scrod t ["{-# RULES \"a\" forall x . id x = x #-}"]
          assertEq t interface.items [itemAt 1 11]

        it t "phase" $ do
          interface <- scrod t ["{-# RULES \"b\" [2] forall x . id x = x #-}"]
          assertEq t interface.items [itemAt 1 11]

        it t "before phase" $ do
          interface <- scrod t ["{-# RULES \"b\" [~2] forall x . id x = x #-}"]
          assertEq t interface.items [itemAt 1 11]

        it t "disabled" $ do
          interface <- scrod t ["{-# RULES \"c\" [~] forall x . id x = x #-}"]
          assertEq t interface.items [itemAt 1 11]

        it t "signature" $ do
          interface <- scrod t ["{-# RULES \"d\" forall (x :: Int) . id x = x #-}"]
          assertEq t interface.items [itemAt 1 11]

        it t "semicolon" $ do
          interface <- scrod t ["{-# RULES \"e\" forall x . id x = x; \"f\" forall x . id x = x #-}"]
          assertEq t interface.items [itemAt 1 11, itemAt 1 36]

    describe t "SpliceD" $ do
      describe t "SpliceDecl" $ do
        it t "untyped" $ do
          interface <- scrod t ["{-# LANGUAGE TemplateHaskell #-} $(pure [])"]
          assertEq t interface.items [itemAt 1 34]

        it t "typed" $ do
          interface <- scrod t ["{-# LANGUAGE TemplateHaskell #-} $$(pure [])"]
          assertEq t interface.items [itemAt 1 34]

    describe t "DocD" $ do
      describe t "DocCommentNamed" $ do
        it t "works" $ do
          interface <- scrod t ["-- $foo", "-- bar"]
          assertEq t interface.items [itemAt 1 1]

      describe t "DocGroup" $ do
        it t "one" $ do
          interface <- scrod t ["-- * one"]
          assertEq t interface.items [itemAt 1 1]

        it t "six" $ do
          interface <- scrod t ["-- ****** six"]
          assertEq t interface.items [itemAt 1 1]

    describe t "RoleAnnotD" $ do
      describe t "RoleAnnotDecl" $ do
        it t "none" $ do
          interface <- scrod t ["data A", "type role A"]
          assertEq t interface.items [itemAt 1 1, itemAt 2 1]

        it t "nominal" $ do
          interface <- scrod t ["data B z", "type role B nominal"]
          assertEq t interface.items [itemAt 1 1, itemAt 2 1]

        it t "representational" $ do
          interface <- scrod t ["data C y", "type role C representational"]
          assertEq t interface.items [itemAt 1 1, itemAt 2 1]

        it t "phantom" $ do
          interface <- scrod t ["data D x", "type role D phantom"]
          assertEq t interface.items [itemAt 1 1, itemAt 2 1]

        it t "inferred" $ do
          interface <- scrod t ["data E w", "type role E _"]
          assertEq t interface.items [itemAt 1 1, itemAt 2 1]

        it t "two" $ do
          interface <- scrod t ["data F u v", "type role F _ _"]
          assertEq t interface.items [itemAt 1 1, itemAt 2 1]

scrod :: (Stack.HasCallStack, Applicative m) => Test m n -> [String] -> m Interface.Interface
scrod t = expectRight t . Main.extract . unlines
