{-# OPTIONS_GHC -O0 #-}

{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Scrod.Unstable.Spec where

import qualified Data.Either as Either
import Data.Function ((&))
import Data.Functor ((<&>))
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
import qualified Scrod.Unstable.Type.Since as Since
import qualified Scrod.Unstable.Type.Subordinates as Subordinates
import qualified Scrod.Unstable.Type.Table as Table
import qualified Scrod.Unstable.Type.TableCell as TableCell
import qualified Scrod.Unstable.Type.TableRow as TableRow
import qualified Scrod.Unstable.Type.Version as Version
import qualified Scrod.Unstable.Type.Warning as Warning

spec :: (MonadFail m, Monad n) => Test m n -> n ()
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

    t.describe "special cases" $ do
      t.it "cpp" $ do
        interface <- scrod t ["{-# language CPP #-}"]
        -- Ensuring we don't get "Cpp", which is the name of the constructor.
        assertEq t (interface.extensions & Map.mapKeys (.value)) $ Map.singleton "CPP" True

      t.it "recursive do" $ do
        interface <- scrod t ["{-# language RecursiveDo #-}"]
        -- Ensuring we don't get "DoRec", which is a deprecated alias.
        assertEq t (interface.extensions & Map.mapKeys (.value)) $ Map.singleton "RecursiveDo" True

      t.it "named field puns" $ do
        interface <- scrod t ["{-# language NamedFieldPuns #-}"]
        -- Ensuring we don't get "RecordPuns", which is a deprecated alias.
        assertEq t (interface.extensions & Map.mapKeys (.value)) $ Map.singleton "NamedFieldPuns" True

      t.it "rank n types" $ do
        interface <- scrod t ["{-# language RankNTypes #-}"]
        -- Ensuring we don't get "PolymorphicComponents" or "Rank2Types", which
        -- are alternative names.
        assertEq t (interface.extensions & Map.mapKeys (.value)) $
          Map.fromList
            [ ("ExplicitForAll", True),
              ("RankNTypes", True)
            ]

  t.describe "documentation" $ do
    t.it "has no documentation by default" $ do
      interface <- scrod t []
      assertEq t interface.documentation Doc.Empty

    t.it "works with block comment before" $ do
      interface <- scrod t ["-- | x", "module M where"]
      assertEq t interface.documentation . Doc.Paragraph $ Doc.String "x"

    t.it "works with inline comment before" $ do
      interface <- scrod t ["{- | x -} module M where"]
      assertEq t interface.documentation . Doc.Paragraph $ Doc.String "x "

    t.it "works with block comment after" $ do
      interface <- scrod t ["module", "-- | x", "M where"]
      assertEq t interface.documentation . Doc.Paragraph $ Doc.String "x"

    t.it "works with inline comment after" $ do
      interface <- scrod t ["module {- | x -} M where"]
      assertEq t interface.documentation . Doc.Paragraph $ Doc.String "x "

    t.it "works with multiple lines" $ do
      interface <- scrod t ["-- | x", "-- y", "module M where"]
      assertEq t interface.documentation . Doc.Paragraph $ Doc.String "x\n y"

    t.it "picks the first comment" $ do
      interface <- scrod t ["-- | x", "module", "-- | y", "M where"]
      assertEq t interface.documentation . Doc.Paragraph $ Doc.String "x"

    t.describe "markup" $ do
      t.it "missing" $ do
        interface <- scrod t ["-- |", "module M where"]
        assertEq t interface.documentation Doc.Empty

      t.it "empty" $ do
        interface <- scrod t ["-- | ", "module M where"]
        assertEq t interface.documentation Doc.Empty

      t.it "string" $ do
        interface <- scrod t ["-- | x", "module M where"]
        assertEq t interface.documentation . Doc.Paragraph $ Doc.String "x"

      t.describe "identifier" $ do
        t.it "apostrophes" $ do
          interface <- scrod t ["-- | 'x'", "module M where"]
          Doc.Paragraph (Doc.Identifier identifier) <- pure interface.documentation
          assertEq t identifier.value "x"

        t.it "grave apostrophe" $ do
          interface <- scrod t ["-- | `x'", "module M where"]
          Doc.Paragraph (Doc.Identifier identifier) <- pure interface.documentation
          assertEq t identifier.value "x"

        t.it "apostrophe grave" $ do
          interface <- scrod t ["-- | 'x`", "module M where"]
          Doc.Paragraph (Doc.Identifier identifier) <- pure interface.documentation
          assertEq t identifier.value "x"

        t.it "graves" $ do
          interface <- scrod t ["-- | `x`", "module M where"]
          Doc.Paragraph (Doc.Identifier identifier) <- pure interface.documentation
          assertEq t identifier.value "x"

        t.it "qualified" $ do
          interface <- scrod t ["-- | 'X.y'", "module M where"]
          Doc.Paragraph (Doc.Identifier identifier) <- pure interface.documentation
          assertEq t identifier.value "X.y"

        t.it "operator" $ do
          interface <- scrod t ["-- | '(%)'", "module M where"]
          Doc.Paragraph (Doc.Identifier identifier) <- pure interface.documentation
          assertEq t identifier.value "(%)"

        t.it "qualified operator" $ do
          interface <- scrod t ["-- | '(X.%)'", "module M where"]
          Doc.Paragraph (Doc.Identifier identifier) <- pure interface.documentation
          assertEq t identifier.value "(X.%)"

        t.it "value" $ do
          interface <- scrod t ["-- | v'X'", "module M where"]
          Doc.Paragraph (Doc.Identifier identifier) <- pure interface.documentation
          assertEq t identifier.namespace $ Just Namespace.Value

        t.it "type" $ do
          interface <- scrod t ["-- | t'X'", "module M where"]
          Doc.Paragraph (Doc.Identifier identifier) <- pure interface.documentation
          assertEq t identifier.namespace $ Just Namespace.Type

      t.describe "module" $ do
        t.it "without label" $ do
          interface <- scrod t ["-- | \"X\"", "module M where"]
          Doc.Paragraph (Doc.Module modLink) <- pure interface.documentation
          assertEq t modLink.name.value "X"

        t.it "with label" $ do
          interface <- scrod t ["-- | [X](\"Y\")", "module M where"]
          Doc.Paragraph (Doc.Module modLink) <- pure interface.documentation
          assertEq t modLink.label . Just $ Doc.String "X"

      t.it "emphasis" $ do
        interface <- scrod t ["-- | /x/", "module M where"]
        assertEq t interface.documentation . Doc.Paragraph . Doc.Emphasis $ Doc.String "x"

      t.it "monospaced" $ do
        -- Note that the line can't be _only_ `@x@` because that would be a
        -- code block rather than some inline monospaced text.
        interface <- scrod t ["-- | x @y@", "module M where"]
        assertEq t interface.documentation . Doc.Paragraph . Doc.Append (Doc.String "x ") . Doc.Monospaced $ Doc.String "y"

      t.it "bold" $ do
        interface <- scrod t ["-- | __x__", "module M where"]
        assertEq t interface.documentation . Doc.Paragraph . Doc.Bold $ Doc.String "x"

      t.describe "unordered list" $ do
        t.it "asterisk" $ do
          interface <- scrod t ["-- | * x", "module M where"]
          assertEq t interface.documentation $ Doc.UnorderedList [Doc.Paragraph $ Doc.String "x"]

        t.it "hyphen" $ do
          interface <- scrod t ["-- | - x", "module M where"]
          assertEq t interface.documentation $ Doc.UnorderedList [Doc.Paragraph $ Doc.String "x"]

      t.describe "ordered list" $ do
        t.it "parentheses" $ do
          interface <- scrod t ["-- | (1) x", "module M where"]
          assertEq t interface.documentation $ Doc.OrderedList [(1, Doc.Paragraph $ Doc.String "x")]

        t.it "period" $ do
          interface <- scrod t ["-- | 1. x", "module M where"]
          assertEq t interface.documentation $ Doc.OrderedList [(1, Doc.Paragraph $ Doc.String "x")]

        t.it "custom number" $ do
          interface <- scrod t ["-- | 2. x", "module M where"]
          assertEq t interface.documentation $ Doc.OrderedList [(2, Doc.Paragraph $ Doc.String "x")]

      t.it "definition list" $ do
        interface <- scrod t ["-- | [x]: y", "module M where"]
        assertEq t interface.documentation $ Doc.DefList [(Doc.String "x", Doc.String "y")]

      t.describe "code block" $ do
        t.it "with inline formatting" $ do
          interface <- scrod t ["-- | @x@", "module M where"]
          assertEq t interface.documentation . Doc.CodeBlock $ Doc.String "x"

        t.it "without inline formatting" $ do
          interface <- scrod t ["-- | > x", "module M where"]
          assertEq t interface.documentation . Doc.CodeBlock $ Doc.String "x"

      t.describe "hyperlink" $ do
        t.it "implicit" $ do
          interface <- scrod t ["-- | http://example", "module M where"]
          Doc.Paragraph (Doc.Hyperlink hyperlink) <- pure interface.documentation
          assertEq t hyperlink.url "http://example"

        t.it "explicit" $ do
          interface <- scrod t ["-- | <http://example>", "module M where"]
          Doc.Paragraph (Doc.Hyperlink hyperlink) <- pure interface.documentation
          assertEq t hyperlink.url "http://example"

        t.it "with label" $ do
          interface <- scrod t ["-- | [x](http://example)", "module M where"]
          Doc.Paragraph (Doc.Hyperlink hyperlink) <- pure interface.documentation
          assertEq t hyperlink.label . Just $ Doc.String "x"

      t.describe "pic" $ do
        t.it "uri" $ do
          interface <- scrod t ["-- | ![x](http://example)", "module M where"]
          Doc.Paragraph (Doc.Pic picture) <- pure interface.documentation
          assertEq t picture.uri "http://example"

        t.it "title" $ do
          interface <- scrod t ["-- | ![x](http://example)", "module M where"]
          Doc.Paragraph (Doc.Pic picture) <- pure interface.documentation
          assertEq t picture.title $ Just "x"

      t.describe "math" $ do
        t.it "inline" $ do
          interface <- scrod t ["-- | \\(x\\)", "module M where"]
          assertEq t interface.documentation . Doc.Paragraph $ Doc.MathInline "x"

        t.it "display" $ do
          interface <- scrod t ["-- | \\[x\\]", "module M where"]
          assertEq t interface.documentation . Doc.Paragraph $ Doc.MathDisplay "x"

      t.it "a name" $ do
        interface <- scrod t ["-- | #x#", "module M where"]
        assertEq t interface.documentation . Doc.Paragraph $ Doc.AName "x"

      t.it "property" $ do
        interface <- scrod t ["-- | prop> x", "module M where"]
        assertEq t interface.documentation $ Doc.Property "x"

      t.describe "examples" $ do
        t.it "one" $ do
          interface <- scrod t ["-- | >>> x", "module M where"]
          Doc.Examples examples <- pure interface.documentation
          assertEq t (examples <&> (.expression)) ["x"]

        t.it "two" $ do
          interface <- scrod t ["-- | >>> x", "-- >>> y", "module M where"]
          Doc.Examples examples <- pure interface.documentation
          assertEq t (examples <&> (.expression)) ["x", "y"]

        t.describe "results" $ do
          t.it "one" $ do
            interface <- scrod t ["-- | >>> x", "-- y", "module M where"]
            Doc.Examples examples <- pure interface.documentation
            assertEq t (examples <&> (.result)) [["y"]]

          t.it "two" $ do
            interface <- scrod t ["-- | >>> x", "-- y", "-- z", "module M where"]
            Doc.Examples examples <- pure interface.documentation
            assertEq t (examples <&> (.result)) [["y", "z"]]

          t.it "blank line" $ do
            interface <- scrod t ["-- | >>> x", "-- <BLANKLINE>", "module M where"]
            Doc.Examples examples <- pure interface.documentation
            assertEq t (examples <&> (.result)) [[""]]

      t.describe "header" $ do
        t.it "title" $ do
          interface <- scrod t ["-- | = x", "module M where"]
          Doc.Header header <- pure interface.documentation
          assertEq t header.title $ Doc.String "x"

        t.describe "level" $ do
          t.it "one" $ do
            interface <- scrod t ["-- | = x", "module M where"]
            Doc.Header header <- pure interface.documentation
            assertEq t header.level Level.One

          t.it "two" $ do
            interface <- scrod t ["-- | == x", "module M where"]
            Doc.Header header <- pure interface.documentation
            assertEq t header.level Level.Two

          t.it "three" $ do
            interface <- scrod t ["-- | === x", "module M where"]
            Doc.Header header <- pure interface.documentation
            assertEq t header.level Level.Three

          t.it "four" $ do
            interface <- scrod t ["-- | ==== x", "module M where"]
            Doc.Header header <- pure interface.documentation
            assertEq t header.level Level.Four

          t.it "five" $ do
            interface <- scrod t ["-- | ===== x", "module M where"]
            Doc.Header header <- pure interface.documentation
            assertEq t header.level Level.Five

          t.it "six" $ do
            interface <- scrod t ["-- | ====== x", "module M where"]
            Doc.Header header <- pure interface.documentation
            assertEq t header.level Level.Six

      t.describe "table" $ do
        t.it "works" $ do
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
                    [ TableRow.MkRow
                        [ TableCell.empty $ Doc.String "a",
                          TableCell.empty $ Doc.String "b"
                        ]
                    ]
                }

        t.it "with header" $ do
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
                    [ TableRow.MkRow
                        [ TableCell.empty $ Doc.String "a",
                          TableCell.empty $ Doc.String "b"
                        ]
                    ],
                  Table.bodyRows =
                    [ TableRow.MkRow
                        [ TableCell.empty $ Doc.String "c",
                          TableCell.empty $ Doc.String "d"
                        ]
                    ]
                }

        t.it "with colspan" $ do
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
                    [ TableRow.MkRow
                        [ TableCell.empty $ Doc.String "a",
                          TableCell.empty $ Doc.String "b"
                        ],
                      TableRow.MkRow
                        [ (TableCell.empty $ Doc.String "c") {TableCell.colspan = 2}
                        ]
                    ]
                }

        t.it "with rowspan" $ do
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
                    [ TableRow.MkRow
                        [ (TableCell.empty $ Doc.String "a\n\n") {TableCell.rowspan = 2},
                          TableCell.empty $ Doc.String "b"
                        ],
                      TableRow.MkRow
                        [ TableCell.empty $ Doc.String "c"
                        ]
                    ]
                }

  t.describe "since" $ do
    t.it "is empty by default" $ do
      interface <- scrod t []
      assertEq t interface.since Nothing

    t.describe "package" $ do
      t.it "works" $ do
        -- The code is in place to populate this field, but Haddock itself does
        -- not handle it. So if we update Haddock and this test starts failing,
        -- simply update the test.
        --
        -- > assertEq t (interface.since.package <&> (.value)) $ Just "p"
        interface <- scrod t ["-- | @since p-0", "module M where"]
        assertEq t interface.documentation $ Doc.Paragraph (Doc.String "@since p-0")

    t.describe "version" $ do
      t.it "parses a simple version" $ do
        interface <- scrod t ["-- | @since 0", "module M where"]
        assertEq t (interface.since <&> (.version.value)) $ Just [0]

      t.it "parses a complex version" $ do
        interface <- scrod t ["-- | @since 1.2", "module M where"]
        assertEq t (interface.since <&> (.version.value)) $ Just [1, 2]

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

  t.describe "exports" $ do
    t.it "has no exports by default" $ do
      interface <- scrod t []
      assertEq t interface.exports Nothing

    t.it "has no exports without module declaration" $ do
      interface <- scrod t ["x = 1"]
      assertEq t interface.exports Nothing

    t.it "has no exports without explicit list" $ do
      interface <- scrod t ["module M where"]
      assertEq t interface.exports Nothing

    t.it "handles empty export list" $ do
      interface <- scrod t ["module M () where"]
      assertEq t interface.exports $ Just []

    -- TODO: Clean up tests from here on down.
    t.describe "var" $ do
      t.it "exports a variable" $ do
        interface <- scrod t ["module M (x) where"]
        assertEq t interface.exports $
          Just [Export.Var ExportName.MkExportName {ExportName.kind = Nothing, ExportName.name = "x"} Nothing Nothing]

      t.it "exports an operator" $ do
        interface <- scrod t ["module M ((<>)) where"]
        assertEq t interface.exports $
          Just [Export.Var ExportName.MkExportName {ExportName.kind = Nothing, ExportName.name = "<>"} Nothing Nothing]

      t.it "exports multiple variables" $ do
        interface <- scrod t ["module M (x, y) where"]
        assertEq t interface.exports $
          Just
            [ Export.Var ExportName.MkExportName {ExportName.kind = Nothing, ExportName.name = "x"} Nothing Nothing,
              Export.Var ExportName.MkExportName {ExportName.kind = Nothing, ExportName.name = "y"} Nothing Nothing
            ]

    t.describe "thing" $ do
      t.it "exports a type without subordinates" $ do
        interface <- scrod t ["module M (T) where"]
        assertEq t interface.exports $
          Just [Export.Thing ExportName.MkExportName {ExportName.kind = Nothing, ExportName.name = "T"} Nothing Nothing Nothing]

      t.it "exports a type with wildcard" $ do
        interface <- scrod t ["module M (T(..)) where"]
        assertEq t interface.exports $
          Just
            [ Export.Thing
                ExportName.MkExportName {ExportName.kind = Nothing, ExportName.name = "T"}
                (Just Subordinates.MkSubordinates {Subordinates.wildcard = True, Subordinates.explicit = []})
                Nothing
                Nothing
            ]

      t.it "exports a type with explicit children" $ do
        interface <- scrod t ["module M (T(A, B)) where"]
        assertEq t interface.exports $
          Just
            [ Export.Thing
                ExportName.MkExportName {ExportName.kind = Nothing, ExportName.name = "T"}
                ( Just
                    Subordinates.MkSubordinates
                      { Subordinates.wildcard = False,
                        Subordinates.explicit =
                          [ ExportName.MkExportName {ExportName.kind = Nothing, ExportName.name = "A"},
                            ExportName.MkExportName {ExportName.kind = Nothing, ExportName.name = "B"}
                          ]
                      }
                )
                Nothing
                Nothing
            ]

      t.it "exports a type with wildcard and explicit children" $ do
        interface <- scrod t ["{-# language PatternSynonyms #-}", "module M (T(.., P)) where"]
        assertEq t interface.exports $
          Just
            [ Export.Thing
                ExportName.MkExportName {ExportName.kind = Nothing, ExportName.name = "T"}
                ( Just
                    Subordinates.MkSubordinates
                      { Subordinates.wildcard = True,
                        Subordinates.explicit =
                          [ ExportName.MkExportName {ExportName.kind = Nothing, ExportName.name = "P"}
                          ]
                      }
                )
                Nothing
                Nothing
            ]

    t.describe "module" $ do
      t.it "re-exports a module" $ do
        interface <- scrod t ["module M (module X) where"]
        assertEq t interface.exports $
          Just [Export.Module ModuleName.MkModuleName {ModuleName.value = "X"} Nothing]

      t.it "re-exports a qualified module" $ do
        interface <- scrod t ["module M (module Data.List) where"]
        assertEq t interface.exports $
          Just [Export.Module ModuleName.MkModuleName {ModuleName.value = "Data.List"} Nothing]

    t.describe "namespace" $ do
      t.it "exports with pattern namespace" $ do
        interface <- scrod t ["{-# language PatternSynonyms #-}", "module M (pattern P) where"]
        assertEq t interface.exports $
          Just [Export.Var ExportName.MkExportName {ExportName.kind = Just ExportNameKind.Pattern, ExportName.name = "P"} Nothing Nothing]

      t.it "exports with type namespace" $ do
        interface <- scrod t ["{-# language ExplicitNamespaces #-}", "module M (type T) where"]
        assertEq t interface.exports $
          Just [Export.Thing ExportName.MkExportName {ExportName.kind = Just ExportNameKind.Type, ExportName.name = "T"} Nothing Nothing Nothing]

    t.describe "warning" $ do
      t.it "attaches warning to var" $ do
        interface <- scrod t ["module M ({-# WARNING \"wrn\" #-} x) where"]
        assertEq t interface.exports $
          Just
            [ Export.Var
                ExportName.MkExportName {ExportName.kind = Nothing, ExportName.name = "x"}
                (Just Warning.MkWarning {Warning.category = Category.MkCategory {Category.value = "deprecations"}, Warning.value = "wrn"})
                Nothing
            ]

      t.it "attaches warning to thing" $ do
        interface <- scrod t ["module M ({-# WARNING \"wrn\" #-} T) where"]
        assertEq t interface.exports $
          Just
            [ Export.Thing
                ExportName.MkExportName {ExportName.kind = Nothing, ExportName.name = "T"}
                Nothing
                (Just Warning.MkWarning {Warning.category = Category.MkCategory {Category.value = "deprecations"}, Warning.value = "wrn"})
                Nothing
            ]

      t.it "attaches warning to thing with subordinates" $ do
        interface <- scrod t ["module M ({-# WARNING \"wrn\" #-} T(..)) where"]
        assertEq t interface.exports $
          Just
            [ Export.Thing
                ExportName.MkExportName {ExportName.kind = Nothing, ExportName.name = "T"}
                (Just Subordinates.MkSubordinates {Subordinates.wildcard = True, Subordinates.explicit = []})
                (Just Warning.MkWarning {Warning.category = Category.MkCategory {Category.value = "deprecations"}, Warning.value = "wrn"})
                Nothing
            ]

      t.it "attaches deprecated to var" $ do
        interface <- scrod t ["module M ({-# DEPRECATED \"dep\" #-} x) where"]
        assertEq t interface.exports $
          Just
            [ Export.Var
                ExportName.MkExportName {ExportName.kind = Nothing, ExportName.name = "x"}
                (Just Warning.MkWarning {Warning.category = Category.MkCategory {Category.value = "deprecations"}, Warning.value = "dep"})
                Nothing
            ]

      t.it "attaches warning with custom category" $ do
        interface <- scrod t ["module M ({-# WARNING in \"x-custom\" \"wrn\" #-} x) where"]
        assertEq t interface.exports $
          Just
            [ Export.Var
                ExportName.MkExportName {ExportName.kind = Nothing, ExportName.name = "x"}
                (Just Warning.MkWarning {Warning.category = Category.MkCategory {Category.value = "x-custom"}, Warning.value = "wrn"})
                Nothing
            ]

      t.it "attaches warning to module re-export" $ do
        interface <- scrod t ["module M ({-# WARNING \"wrn\" #-} module X) where"]
        assertEq t interface.exports $
          Just
            [ Export.Module
                ModuleName.MkModuleName {ModuleName.value = "X"}
                (Just Warning.MkWarning {Warning.category = Category.MkCategory {Category.value = "deprecations"}, Warning.value = "wrn"})
            ]

    t.describe "documentation" $ do
      t.it "handles section heading" $ do
        interface <- scrod t ["module M ( -- * Section", ") where"]
        assertEq t interface.exports $
          Just
            [ Export.Group Level.One (Doc.Paragraph (Doc.String "Section"))
            ]

      t.it "handles section heading level two" $ do
        interface <- scrod t ["module M ( -- ** Section", ") where"]
        assertEq t interface.exports $
          Just
            [ Export.Group Level.Two (Doc.Paragraph (Doc.String "Section"))
            ]

      t.it "handles inline doc" $ do
        interface <- scrod t ["module M ( -- | Some doc", ") where"]
        assertEq t interface.exports $
          Just
            [ Export.Doc (Doc.Paragraph (Doc.String "Some doc"))
            ]

      t.it "handles doc before export" $ do
        interface <- scrod t ["module M ( -- | foo", " bar ) where"]
        assertEq t interface.exports $
          Just
            [ Export.Doc (Doc.Paragraph (Doc.String "foo")),
              Export.Var ExportName.MkExportName {ExportName.kind = Nothing, ExportName.name = "bar"} Nothing Nothing
            ]

      t.it "handles named doc reference" $ do
        interface <- scrod t ["module M ( -- $chunkName", ") where"]
        assertEq t interface.exports $
          Just
            [ Export.DocNamed "chunkName"
            ]

      t.it "handles doc attached to export" $ do
        interface <- scrod t ["module M ( x -- ^ y", ") where"]
        assertEq t interface.exports $
          Just
            [ Export.Var
                ExportName.MkExportName {ExportName.kind = Nothing, ExportName.name = "x"}
                Nothing
                (Just (Doc.Paragraph (Doc.String "y")))
            ]

  t.describe "HsDecl" $ do
    let itemAt l c = Located.MkLocated
          { Located.location = Location.MkLocation
              { Location.line = Line.MkLine l
              , Location.column = Column.MkColumn c
              }
          , Located.value = Item.MkItem
          }

    t.describe "TyClD" $ do
      t.describe "FamDecl" $ do
        t.it "type family open" $ do
          interface <- scrod t ["type family F a"]
          assertEq t interface.items [itemAt 1 1]

        t.it "type family open multiple params" $ do
          interface <- scrod t ["type family F a b"]
          assertEq t interface.items [itemAt 1 1]

        t.it "type family closed" $ do
          interface <- scrod t ["type family G a where { G Int = Bool; G a = Char }"]
          assertEq t interface.items [itemAt 1 1]

        t.it "type family closed empty" $ do
          interface <- scrod t ["type family H a where {}"]
          assertEq t interface.items [itemAt 1 1]

        t.it "type family with kind sig" $ do
          interface <- scrod t ["type family I a :: Type"]
          assertEq t interface.items [itemAt 1 1]

        t.it "type family with result kind variable" $ do
          interface <- scrod t ["type family I2 a = (r :: Type)"]
          assertEq t interface.items [itemAt 1 1]

        t.it "type family with injectivity" $ do
          interface <- scrod t ["type family J a = r | r -> a"]
          assertEq t interface.items [itemAt 1 1]

        t.it "data family" $ do
          interface <- scrod t ["data family D a"]
          assertEq t interface.items [itemAt 1 1]

        t.it "data family with kind sig" $ do
          interface <- scrod t ["data family E a :: Type"]
          assertEq t interface.items [itemAt 1 1]

      t.describe "SynDecl" $ do
        t.it "basic" $ do
          interface <- scrod t ["type S = Int"]
          assertEq t interface.items [itemAt 1 1]

        t.it "with params" $ do
          interface <- scrod t ["type T a = [a]"]
          assertEq t interface.items [itemAt 1 1]

        t.it "with multiple params" $ do
          interface <- scrod t ["type U a b = Either a b"]
          assertEq t interface.items [itemAt 1 1]

        t.it "with kind annotation" $ do
          interface <- scrod t ["type V (a :: Type) = Maybe a"]
          assertEq t interface.items [itemAt 1 1]

        t.it "with forall" $ do
          interface <- scrod t ["type R = forall a. a -> a"]
          assertEq t interface.items [itemAt 1 1]

        t.it "type operator" $ do
          interface <- scrod t ["type a + b = Either a b"]
          assertEq t interface.items [itemAt 1 1]

      t.describe "DataDecl" $ do
        t.it "data basic" $ do
          interface <- scrod t ["data A"]
          assertEq t interface.items [itemAt 1 1]

        t.it "data with constructor" $ do
          interface <- scrod t ["data B = B"]
          assertEq t interface.items [itemAt 1 1]

        t.it "data with multiple constructors" $ do
          interface <- scrod t ["data C = C1 | C2 | C3"]
          assertEq t interface.items [itemAt 1 1]

        t.it "data with fields" $ do
          interface <- scrod t ["data D = D Int Bool"]
          assertEq t interface.items [itemAt 1 1]

        t.it "data with record" $ do
          interface <- scrod t ["data E = E { eInt :: Int, eBool :: Bool }"]
          assertEq t interface.items [itemAt 1 1]

        t.it "data with deriving" $ do
          interface <- scrod t ["data F = F deriving Show"]
          assertEq t interface.items [itemAt 1 1]

        t.it "data with deriving multiple" $ do
          interface <- scrod t ["data G = G deriving (Show, Eq)"]
          assertEq t interface.items [itemAt 1 1]

        t.it "data with deriving strategies" $ do
          interface <- scrod t ["data H = H deriving stock Show"]
          assertEq t interface.items [itemAt 1 1]

        t.it "data with deriving via" $ do
          interface <- scrod t ["newtype I = I Int deriving Show via Int"]
          assertEq t interface.items [itemAt 1 1]

        t.it "data with deriving anyclass" $ do
          interface <- scrod t ["data J = J deriving anyclass C"]
          assertEq t interface.items [itemAt 1 1]

        t.it "data with deriving newtype" $ do
          interface <- scrod t ["newtype K = K Int deriving newtype Num"]
          assertEq t interface.items [itemAt 1 1]

        t.it "data with type params" $ do
          interface <- scrod t ["data L a = L a"]
          assertEq t interface.items [itemAt 1 1]

        t.it "data with phantom type" $ do
          interface <- scrod t ["data M a = M"]
          assertEq t interface.items [itemAt 1 1]

        t.it "data with kind sig" $ do
          interface <- scrod t ["data N (a :: Type) = N"]
          assertEq t interface.items [itemAt 1 1]

        t.it "data with existential" $ do
          interface <- scrod t ["data O = forall a . Show a => O a"]
          assertEq t interface.items [itemAt 1 1]

        t.it "data with GADT" $ do
          interface <- scrod t ["{-# LANGUAGE GADTs #-} data P a where P1 :: P Int; P2 :: P Bool"]
          assertEq t interface.items [itemAt 1 24]

        t.it "data with GADT record" $ do
          interface <- scrod t ["{-# LANGUAGE GADTs #-} data Q a where Q :: { qVal :: Int } -> Q Int"]
          assertEq t interface.items [itemAt 1 24]

        t.it "data with strictness" $ do
          interface <- scrod t ["data R = R !Int"]
          assertEq t interface.items [itemAt 1 1]

        t.it "data with laziness" $ do
          interface <- scrod t ["data S = S ~Int"]
          assertEq t interface.items [itemAt 1 1]

        t.it "data with unpack" $ do
          interface <- scrod t ["{-# LANGUAGE UnboxedTuples #-} data T = T {-# UNPACK #-} !Int"]
          assertEq t interface.items [itemAt 1 32]

        t.it "data with nounpack" $ do
          interface <- scrod t ["data U = U {-# NOUNPACK #-} !Int"]
          assertEq t interface.items [itemAt 1 1]

        t.it "newtype basic" $ do
          interface <- scrod t ["newtype V = V Int"]
          assertEq t interface.items [itemAt 1 1]

        t.it "newtype with record" $ do
          interface <- scrod t ["newtype W = W { unW :: Int }"]
          assertEq t interface.items [itemAt 1 1]

        t.it "data with context" $ do
          interface <- scrod t ["{-# LANGUAGE DatatypeContexts #-} data Eq a => X a = X a"]
          assertEq t interface.items [itemAt 1 35]

        t.it "data with forall" $ do
          interface <- scrod t ["{-# LANGUAGE ExplicitForAll #-} data Y = forall a . Y a"]
          assertEq t interface.items [itemAt 1 33]

        t.it "data with linear field" $ do
          interface <- scrod t ["{-# LANGUAGE LinearTypes #-} data Z a b = Z (a %1 -> b)"]
          assertEq t interface.items [itemAt 1 30]

        t.it "data with type operator name" $ do
          interface <- scrod t ["{-# LANGUAGE TypeOperators #-} data a :+: b = L a | R b"]
          assertEq t interface.items [itemAt 1 32]

        t.it "data with multiple deriving clauses" $ do
          interface <- scrod t ["data AA = AA deriving Show deriving Eq"]
          assertEq t interface.items [itemAt 1 1]

        t.it "type data" $ do
          interface <- scrod t ["{-# LANGUAGE TypeData #-} type data TBool = TTrue | TFalse"]
          assertEq t interface.items [itemAt 1 27]

      t.describe "ClassDecl" $ do
        t.it "basic" $ do
          interface <- scrod t ["class Cls a"]
          assertEq t interface.items [itemAt 1 1]

        t.it "with method" $ do
          interface <- scrod t ["class Cls a where method :: a -> a"]
          assertEq t interface.items [itemAt 1 1]

        t.it "with default method" $ do
          interface <- scrod t ["class Cls a where { method :: a -> a; method = id }"]
          assertEq t interface.items [itemAt 1 1]

        t.it "with multiple methods" $ do
          interface <- scrod t ["class Cls a where { m1 :: a; m2 :: a -> a }"]
          assertEq t interface.items [itemAt 1 1]

        t.it "with superclass" $ do
          interface <- scrod t ["class Eq a => Cls a"]
          assertEq t interface.items [itemAt 1 1]

        t.it "with multiple superclasses" $ do
          interface <- scrod t ["class (Eq a, Ord a) => Cls a"]
          assertEq t interface.items [itemAt 1 1]

        t.it "with fundeps" $ do
          interface <- scrod t ["{-# LANGUAGE FunctionalDependencies #-} class Cls a b | a -> b"]
          assertEq t interface.items [itemAt 1 41]

        t.it "with fundeps multiple" $ do
          interface <- scrod t ["{-# LANGUAGE FunctionalDependencies #-} class Cls a b c | a -> b, b -> c"]
          assertEq t interface.items [itemAt 1 41]

        t.it "with associated type" $ do
          interface <- scrod t ["{-# LANGUAGE TypeFamilies #-} class Cls a where type T a"]
          assertEq t interface.items [itemAt 1 31]

        t.it "with associated type with kind sig" $ do
          interface <- scrod t ["{-# LANGUAGE TypeFamilies #-} class Cls a where type T a :: Type -> Type"]
          assertEq t interface.items [itemAt 1 31]

        t.it "with associated type default" $ do
          interface <- scrod t ["{-# LANGUAGE TypeFamilies #-} class Cls a where { type T a; type T a = Int }"]
          assertEq t interface.items [itemAt 1 31]

        t.it "with associated data" $ do
          interface <- scrod t ["{-# LANGUAGE TypeFamilies #-} class Cls a where data D a"]
          assertEq t interface.items [itemAt 1 31]

        t.it "with associated data with kind sig" $ do
          interface <- scrod t ["{-# LANGUAGE TypeFamilies #-} class Cls a where data D a :: Type"]
          assertEq t interface.items [itemAt 1 31]

        t.it "with quantified constraint" $ do
          interface <- scrod t ["{-# LANGUAGE QuantifiedConstraints #-} class (forall x. Eq (f x)) => Cls f"]
          assertEq t interface.items [itemAt 1 40]

        t.it "with default signature" $ do
          interface <- scrod t ["{-# LANGUAGE DefaultSignatures #-} class Cls a where { method :: a -> a; default method :: Show a => a -> a }"]
          assertEq t interface.items [itemAt 1 36]

        t.it "with minimal pragma" $ do
          interface <- scrod t ["class Cls a where { m1 :: a; m2 :: a; {-# MINIMAL m1 | m2 #-} }"]
          assertEq t interface.items [itemAt 1 1]

        t.it "with kind sig" $ do
          interface <- scrod t ["{-# LANGUAGE KindSignatures #-} class Cls (a :: Type)"]
          assertEq t interface.items [itemAt 1 33]

        t.it "no params" $ do
          interface <- scrod t ["class Cls"]
          assertEq t interface.items [itemAt 1 1]

        t.it "multi-param" $ do
          interface <- scrod t ["{-# LANGUAGE MultiParamTypeClasses #-} class Cls a b"]
          assertEq t interface.items [itemAt 1 40]

    t.describe "InstD" $ do
      t.describe "ClsInstD" $ do
        t.it "no params" $ do
          interface <- scrod t ["instance Cls where"]
          assertEq t interface.items [itemAt 1 1]

        t.it "basic" $ do
          interface <- scrod t ["instance Cls Int where"]
          assertEq t interface.items [itemAt 1 1]

        t.it "multi params" $ do
          interface <- scrod t ["{-# LANGUAGE MultiParamTypeClasses #-} instance Cls Int Bool where"]
          assertEq t interface.items [itemAt 1 40]

        t.it "with method" $ do
          interface <- scrod t ["instance Cls Int where method = id"]
          assertEq t interface.items [itemAt 1 1]

        t.it "with type application" $ do
          interface <- scrod t ["instance Cls [a] where"]
          assertEq t interface.items [itemAt 1 1]

        t.it "with context" $ do
          interface <- scrod t ["instance Eq a => Cls [a] where"]
          assertEq t interface.items [itemAt 1 1]

        t.it "with multiple contexts" $ do
          interface <- scrod t ["instance (Eq a, Ord a) => Cls [a] where"]
          assertEq t interface.items [itemAt 1 1]

        t.it "overlapping" $ do
          interface <- scrod t ["{-# LANGUAGE FlexibleInstances #-} instance {-# OVERLAPPING #-} Cls [Int] where"]
          assertEq t interface.items [itemAt 1 36]

        t.it "overlappable" $ do
          interface <- scrod t ["{-# LANGUAGE FlexibleInstances #-} instance {-# OVERLAPPABLE #-} Cls [a] where"]
          assertEq t interface.items [itemAt 1 36]

        t.it "overlaps" $ do
          interface <- scrod t ["{-# LANGUAGE FlexibleInstances #-} instance {-# OVERLAPS #-} Cls [Int] where"]
          assertEq t interface.items [itemAt 1 36]

        t.it "incoherent" $ do
          interface <- scrod t ["{-# LANGUAGE FlexibleInstances #-} instance {-# INCOHERENT #-} Cls [Int] where"]
          assertEq t interface.items [itemAt 1 36]

        t.it "with explicit forall" $ do
          interface <- scrod t ["{-# LANGUAGE ExplicitForAll, FlexibleInstances #-} instance forall a. Cls [a] where"]
          assertEq t interface.items [itemAt 1 52]

        t.it "with type application" $ do
          interface <- scrod t ["{-# LANGUAGE TypeApplications #-} instance Cls @Type Int where"]
          assertEq t interface.items [itemAt 1 35]

      t.describe "DataFamInstD" $ do
        t.it "basic" $ do
          interface <- scrod t ["data family D a; data instance D Int = DInt"]
          assertEq t interface.items [itemAt 1 1, itemAt 1 18]

        t.it "with constructor" $ do
          interface <- scrod t ["data family D a; data instance D Bool = DBool Bool"]
          assertEq t interface.items [itemAt 1 1, itemAt 1 18]

        t.it "with multiple constructors" $ do
          interface <- scrod t ["data family D a; data instance D Char = DC1 | DC2 Char"]
          assertEq t interface.items [itemAt 1 1, itemAt 1 18]

        t.it "with record" $ do
          interface <- scrod t ["data family D a; data instance D () = DUnit { dUnit :: () }"]
          assertEq t interface.items [itemAt 1 1, itemAt 1 18]

        t.it "newtype instance" $ do
          interface <- scrod t ["data family D a; newtype instance D Float = DFloat Float"]
          assertEq t interface.items [itemAt 1 1, itemAt 1 18]

        t.it "with GADT" $ do
          interface <- scrod t ["{-# LANGUAGE GADTs #-} data family D a; data instance D (Maybe a) where DMaybe :: a -> D (Maybe a)"]
          assertEq t interface.items [itemAt 1 24, itemAt 1 41]

        t.it "with forall" $ do
          interface <- scrod t ["data family D a; data instance forall a . D [a] = DList [a]"]
          assertEq t interface.items [itemAt 1 1, itemAt 1 18]

        t.it "with deriving" $ do
          interface <- scrod t ["data family D a; data instance D Word = DWord Word deriving Show"]
          assertEq t interface.items [itemAt 1 1, itemAt 1 18]

      t.describe "TyFamInstD" $ do
        t.it "basic" $ do
          interface <- scrod t ["type family F a; type instance F Int = Bool"]
          assertEq t interface.items [itemAt 1 1, itemAt 1 18]

        t.it "with type var" $ do
          interface <- scrod t ["type family F a; type instance F [a] = a"]
          assertEq t interface.items [itemAt 1 1, itemAt 1 18]

        t.it "with nested" $ do
          interface <- scrod t ["type family F a; type instance F (Maybe a) = Either () a"]
          assertEq t interface.items [itemAt 1 1, itemAt 1 18]

        t.it "with forall" $ do
          interface <- scrod t ["type family F a; type instance forall a. F [a] = a"]
          assertEq t interface.items [itemAt 1 1, itemAt 1 18]

    t.describe "DerivD" $ do
      t.describe "DerivDecl" $ do
        t.it "basic" $ do
          interface <- scrod t ["data A = A; deriving instance Show A"]
          assertEq t interface.items [itemAt 1 1, itemAt 1 13]

        t.it "with context" $ do
          interface <- scrod t ["data B a = B a; deriving instance Show a => Show (B a)"]
          assertEq t interface.items [itemAt 1 1, itemAt 1 17]

        t.it "stock" $ do
          interface <- scrod t ["{-# LANGUAGE DerivingStrategies #-} data C = C; deriving stock instance Show C"]
          assertEq t interface.items [itemAt 1 37, itemAt 1 49]

        t.it "newtype" $ do
          interface <- scrod t ["{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving #-} newtype D = D Int; deriving newtype instance Num D"]
          assertEq t interface.items [itemAt 1 65, itemAt 1 84]

        t.it "anyclass" $ do
          interface <- scrod t ["{-# LANGUAGE DerivingStrategies, DeriveAnyClass #-} class Cls a; data E = E; deriving anyclass instance Cls E"]
          assertEq t interface.items [itemAt 1 53, itemAt 1 66, itemAt 1 78]

        t.it "via" $ do
          interface <- scrod t ["{-# LANGUAGE DerivingStrategies, DerivingVia, StandaloneDeriving #-} newtype F = F Int; deriving via Int instance Show F"]
          assertEq t interface.items [itemAt 1 70, itemAt 1 89]

    t.describe "ValD" $ do
      t.describe "FunBind" $ do
        t.it "basic" $ do
          interface <- scrod t ["f x = x"]
          assertEq t interface.items [itemAt 1 1]

        t.it "with multiple args" $ do
          interface <- scrod t ["f x y z = x"]
          assertEq t interface.items [itemAt 1 1]

        t.it "with pattern matching" $ do
          interface <- scrod t ["f [] = 0; f (x:xs) = 1 + f xs"]
          assertEq t interface.items [itemAt 1 1]

        t.it "with guards" $ do
          interface <- scrod t ["f x | x > 0 = 1 | otherwise = 0"]
          assertEq t interface.items [itemAt 1 1]

        t.it "with where" $ do
          interface <- scrod t ["f x = y where y = x + 1"]
          assertEq t interface.items [itemAt 1 1]

        t.it "with let" $ do
          interface <- scrod t ["f x = let y = x + 1 in y"]
          assertEq t interface.items [itemAt 1 1]

        t.it "operator" $ do
          interface <- scrod t ["x +++ y = x + y"]
          assertEq t interface.items [itemAt 1 1]

        t.it "operator prefix" $ do
          interface <- scrod t ["(+++) x y = x + y"]
          assertEq t interface.items [itemAt 1 1]

        t.it "infix" $ do
          interface <- scrod t [" x `plus` y = x + y "]
          assertEq t interface.items [itemAt 1 2]

        t.it "with view pattern" $ do
          interface <- scrod t ["{-# LANGUAGE ViewPatterns #-} f (show -> s) = s"]
          assertEq t interface.items [itemAt 1 31]

        t.it "with pattern guard" $ do
          interface <- scrod t ["f x | Just y <- g x = y | otherwise = x"]
          assertEq t interface.items [itemAt 1 1]

        t.it "with type abstraction" $ do
          interface <- scrod t ["{-# LANGUAGE TypeAbstractions #-} f @a (x :: a) = x"]
          assertEq t interface.items [itemAt 1 35]

      t.describe "PatBind" $ do
        t.it "simple" $ do
          interface <- scrod t ["x = 1"]
          assertEq t interface.items [itemAt 1 1]

        t.it "tuple" $ do
          interface <- scrod t ["(a, b) = (1, 2)"]
          assertEq t interface.items [itemAt 1 1]

        t.it "list" $ do
          interface <- scrod t ["[x, y] = [1, 2]"]
          assertEq t interface.items [itemAt 1 1]

        t.it "cons" $ do
          interface <- scrod t ["(h:t) = [1, 2, 3]"]
          assertEq t interface.items [itemAt 1 1]

        t.it "record" $ do
          interface <- scrod t ["data T = C { f :: Int }; C { f = x } = C 1"]
          assertEq t interface.items [itemAt 1 1, itemAt 1 26]

        t.it "as pattern" $ do
          interface <- scrod t ["all@(x:xs) = [1, 2, 3]"]
          assertEq t interface.items [itemAt 1 1]

        t.it "wildcard" $ do
          interface <- scrod t ["_ = undefined"]
          assertEq t interface.items [itemAt 1 1]

        t.it "lazy" $ do
          interface <- scrod t ["~(a, b) = undefined"]
          assertEq t interface.items [itemAt 1 1]

        t.it "bang" $ do
          interface <- scrod t ["{-# LANGUAGE BangPatterns #-} !x = 1"]
          assertEq t interface.items [itemAt 1 31]

        t.it "with type signature" $ do
          interface <- scrod t ["{-# LANGUAGE ScopedTypeVariables #-} (x :: Int) = 1"]
          assertEq t interface.items [itemAt 1 38]

        t.it "with view pattern" $ do
          interface <- scrod t ["{-# LANGUAGE ViewPatterns #-} (reverse -> xs) = [1,2,3]"]
          assertEq t interface.items [itemAt 1 31]

      t.describe "PatSynBind" $ do
        t.it "unidirectional" $ do
          interface <- scrod t ["{-# LANGUAGE PatternSynonyms #-} pattern P x = Just x"]
          assertEq t interface.items [itemAt 1 34]

        t.it "bidirectional implicit" $ do
          interface <- scrod t ["{-# LANGUAGE PatternSynonyms #-} pattern Q x = (x, x)"]
          assertEq t interface.items [itemAt 1 34]

        t.it "bidirectional explicit" $ do
          interface <- scrod t ["{-# LANGUAGE PatternSynonyms #-} pattern R x <- Just x where R x = Just x"]
          assertEq t interface.items [itemAt 1 34]

        t.it "view pattern" $ do
          interface <- scrod t ["{-# LANGUAGE PatternSynonyms, ViewPatterns #-} pattern S x <- (show -> x)"]
          assertEq t interface.items [itemAt 1 48]

        t.it "record" $ do
          interface <- scrod t ["{-# LANGUAGE PatternSynonyms #-} pattern T { tField } = Just tField"]
          assertEq t interface.items [itemAt 1 34]

        t.it "prefix" $ do
          interface <- scrod t ["{-# LANGUAGE PatternSynonyms #-} pattern P x = Just x"]
          assertEq t interface.items [itemAt 1 34]

        t.it "infix" $ do
          interface <- scrod t ["{-# LANGUAGE PatternSynonyms #-} pattern x :+: y = (x, y)"]
          assertEq t interface.items [itemAt 1 34]

    t.describe "SigD" $ do
      t.describe "TypeSig" $ do
        t.it "basic" $ do
          interface <- scrod t ["f :: Int"]
          assertEq t interface.items [itemAt 1 1]

        t.it "with args" $ do
          interface <- scrod t ["f :: Int -> Bool"]
          assertEq t interface.items [itemAt 1 1]

        t.it "with implicit forall" $ do
          interface <- scrod t ["f :: a -> a"]
          assertEq t interface.items [itemAt 1 1]

        t.it "with explicit forall visible" $ do
          interface <- scrod t ["f :: forall a . a -> a"]
          assertEq t interface.items [itemAt 1 1]

        t.it "with explicit forall invisible" $ do
          interface <- scrod t ["f :: forall {a} . a -> a"]
          assertEq t interface.items [itemAt 1 1]

        t.it "with context" $ do
          interface <- scrod t ["f :: Eq a => a -> Bool"]
          assertEq t interface.items [itemAt 1 1]

        t.it "with multiple names" $ do
          interface <- scrod t ["f, g :: Int"]
          assertEq t interface.items [itemAt 1 1]

        t.it "with kind sig" $ do
          interface <- scrod t ["{-# LANGUAGE KindSignatures #-} f :: forall (a :: Type) . a -> a"]
          assertEq t interface.items [itemAt 1 33]

        t.it "with linear arrow" $ do
          interface <- scrod t ["{-# LANGUAGE LinearTypes #-} f :: a %1 -> b"]
          assertEq t interface.items [itemAt 1 30]

        t.it "with multiplicity poly" $ do
          interface <- scrod t ["{-# LANGUAGE LinearTypes #-} f :: a %m -> b"]
          assertEq t interface.items [itemAt 1 30]

        t.it "with visible forall" $ do
          interface <- scrod t ["{-# LANGUAGE RequiredTypeArguments #-} f :: forall a -> a -> a"]
          assertEq t interface.items [itemAt 1 40]

      t.describe "PatSynSig" $ do
        t.it "basic" $ do
          interface <- scrod t ["{-# LANGUAGE PatternSynonyms #-} pattern P :: Int -> Maybe Int"]
          assertEq t interface.items [itemAt 1 34]

        t.it "with forall provided" $ do
          interface <- scrod t ["{-# LANGUAGE PatternSynonyms #-} pattern Q :: forall a . a -> Maybe a"]
          assertEq t interface.items [itemAt 1 34]

        t.it "with forall required" $ do
          interface <- scrod t ["{-# LANGUAGE PatternSynonyms, ExplicitForAll #-} pattern R :: forall a . Show a => a -> Maybe a"]
          assertEq t interface.items [itemAt 1 50]

        t.it "bidirectional" $ do
          interface <- scrod t ["{-# LANGUAGE PatternSynonyms #-} pattern S :: forall a . () => (Eq a) => a -> Maybe a"]
          assertEq t interface.items [itemAt 1 34]

      t.describe "ClassOpSig" $ do
        t.it "basic" $ do
          interface <- scrod t ["class C a where op :: a -> a"]
          assertEq t interface.items [itemAt 1 1]

        t.it "default" $ do
          interface <- scrod t ["class C a where { op :: a -> a; default op :: a -> a }"]
          assertEq t interface.items [itemAt 1 1]

        t.it "multiple" $ do
          interface <- scrod t ["class C a where op1, op2 :: a -> a"]
          assertEq t interface.items [itemAt 1 1]

      t.describe "FixSig" $ do
        t.it "infixl" $ do
          interface <- scrod t ["infixl 6 +++"]
          assertEq t interface.items [itemAt 1 1]

        t.it "infixr" $ do
          interface <- scrod t ["infixr 5 +++"]
          assertEq t interface.items [itemAt 1 1]

        t.it "infix" $ do
          interface <- scrod t ["infix 4 +++"]
          assertEq t interface.items [itemAt 1 1]

        t.it "non-operator" $ do
          interface <- scrod t [" infix 5 `T` "]
          assertEq t interface.items [itemAt 1 2]

        t.it "multiple" $ do
          interface <- scrod t ["infixl 6 #, %"]
          assertEq t interface.items [itemAt 1 1]

        t.it "precedence 0" $ do
          interface <- scrod t ["infixl 0 +++"]
          assertEq t interface.items [itemAt 1 1]

        t.it "precedence 9" $ do
          interface <- scrod t ["infixr 9 +++"]
          assertEq t interface.items [itemAt 1 1]

      t.describe "InlineSig" $ do
        t.it "inline" $ do
          interface <- scrod t ["{-# INLINE f #-}"]
          assertEq t interface.items [itemAt 1 1]

        t.it "inline operator" $ do
          interface <- scrod t ["{-# INLINE (+++) #-}"]
          assertEq t interface.items [itemAt 1 1]

        t.it "conlike" $ do
          interface <- scrod t ["{-# INLINE CONLIKE f #-}"]
          assertEq t interface.items [itemAt 1 1]

        t.it "noinline" $ do
          interface <- scrod t ["{-# NOINLINE f #-}"]
          assertEq t interface.items [itemAt 1 1]

        t.it "inlinable" $ do
          interface <- scrod t ["{-# INLINABLE f #-}"]
          assertEq t interface.items [itemAt 1 1]

        t.it "inline phase" $ do
          interface <- scrod t ["{-# INLINE [2] f #-}"]
          assertEq t interface.items [itemAt 1 1]

        t.it "inline before phase" $ do
          interface <- scrod t ["{-# INLINE [~2] f #-}"]
          assertEq t interface.items [itemAt 1 1]

        t.it "opaque" $ do
          interface <- scrod t ["{-# OPAQUE f #-}"]
          assertEq t interface.items [itemAt 1 1]

      t.describe "SpecSig" $ do
        t.it "basic" $ do
          interface <- scrod t ["{-# SPECIALIZE f :: Int -> Int #-}"]
          assertEq t interface.items [itemAt 1 1]

        t.it "uk" $ do
          interface <- scrod t ["{-# SPECIALISE f :: Int -> Int #-}"]
          assertEq t interface.items [itemAt 1 1]

        t.it "with inline" $ do
          interface <- scrod t ["{-# SPECIALIZE INLINE f :: Int -> Int #-}"]
          assertEq t interface.items [itemAt 1 1]

        t.it "with noinline" $ do
          interface <- scrod t ["{-# SPECIALIZE NOINLINE f :: Int -> Int #-}"]
          assertEq t interface.items [itemAt 1 1]

        t.it "with phase" $ do
          interface <- scrod t ["{-# SPECIALIZE [1] f :: Int -> Int #-}"]
          assertEq t interface.items [itemAt 1 1]

        t.it "with before phase" $ do
          interface <- scrod t ["{-# SPECIALIZE [~1] f :: Int -> Int #-}"]
          assertEq t interface.items [itemAt 1 1]

      t.describe "SpecInstSig" $ do
        t.it "basic" $ do
          interface <- scrod t ["instance Cls [a] where {-# SPECIALIZE instance Cls [Int] #-}"]
          assertEq t interface.items [itemAt 1 1]

        t.it "with context" $ do
          interface <- scrod t ["{-# SPECIALIZE instance Eq a => Cls [a] #-}"]
          assertEq t interface.items [itemAt 1 1]

      t.describe "MinimalSig" $ do
        t.it "single" $ do
          interface <- scrod t ["class C a where { m :: a; {-# MINIMAL m #-} }"]
          assertEq t interface.items [itemAt 1 1]

        t.it "or" $ do
          interface <- scrod t ["class C a where { m1, m2 :: a; {-# MINIMAL m1 | m2 #-} }"]
          assertEq t interface.items [itemAt 1 1]

        t.it "and" $ do
          interface <- scrod t ["class C a where { m1, m2 :: a; {-# MINIMAL m1, m2 #-} }"]
          assertEq t interface.items [itemAt 1 1]

        t.it "nested" $ do
          interface <- scrod t ["class C a where { m1, m2, m3 :: a; {-# MINIMAL (m1, m2) | m3 #-} }"]
          assertEq t interface.items [itemAt 1 1]

        t.it "empty" $ do
          interface <- scrod t ["class C a where { m :: a; m = undefined; {-# MINIMAL #-} }"]
          assertEq t interface.items [itemAt 1 1]

      t.describe "SCCFunSig" $ do
        t.it "basic" $ do
          interface <- scrod t ["{-# SCC f #-}"]
          assertEq t interface.items [itemAt 1 1]

        t.it "with string" $ do
          interface <- scrod t ["{-# SCC f \"cost-centre-name\" #-}"]
          assertEq t interface.items [itemAt 1 1]

      t.describe "CompleteMatchSig" $ do
        t.it "basic" $ do
          interface <- scrod t ["{-# COMPLETE P #-}"]
          assertEq t interface.items [itemAt 1 1]

        t.it "multiple" $ do
          interface <- scrod t ["{-# COMPLETE P, Q #-}"]
          assertEq t interface.items [itemAt 1 1]

        t.it "with type" $ do
          interface <- scrod t ["{-# COMPLETE P :: Maybe #-}"]
          assertEq t interface.items [itemAt 1 1]

        t.it "with type multiple" $ do
          interface <- scrod t ["{-# COMPLETE P, Q :: Either #-}"]
          assertEq t interface.items [itemAt 1 1]

    t.describe "KindSigD" $ do
      t.describe "StandaloneKindSig" $ do
        t.it "basic" $ do
          interface <- scrod t ["{-# LANGUAGE StandaloneKindSignatures #-} type T :: Type; data T"]
          assertEq t interface.items [itemAt 1 43, itemAt 1 59]

        t.it "with arrow" $ do
          interface <- scrod t ["{-# LANGUAGE StandaloneKindSignatures #-} type T :: Type -> Type; data T a"]
          assertEq t interface.items [itemAt 1 43, itemAt 1 67]

        t.it "with constraint" $ do
          interface <- scrod t ["{-# LANGUAGE StandaloneKindSignatures #-} type C :: Type -> Constraint; class C a"]
          assertEq t interface.items [itemAt 1 43, itemAt 1 73]

        t.it "with poly kind" $ do
          interface <- scrod t ["{-# LANGUAGE StandaloneKindSignatures, PolyKinds #-} type T :: forall k . k -> Type; data T a"]
          assertEq t interface.items [itemAt 1 54, itemAt 1 86]

        t.it "for type family" $ do
          interface <- scrod t ["{-# LANGUAGE StandaloneKindSignatures, TypeFamilies #-} type F :: Type -> Type; type family F a"]
          assertEq t interface.items [itemAt 1 57, itemAt 1 81]

        t.it "for type synonym" $ do
          interface <- scrod t ["{-# LANGUAGE StandaloneKindSignatures #-} type S :: Type -> Type; type S a = [a]"]
          assertEq t interface.items [itemAt 1 43, itemAt 1 67]

        t.it "for data family" $ do
          interface <- scrod t ["{-# LANGUAGE StandaloneKindSignatures, TypeFamilies #-} type D :: Type -> Type; data family D a"]
          assertEq t interface.items [itemAt 1 57, itemAt 1 81]

    t.describe "DefD" $ do
      t.describe "DefaultDecl" $ do
        t.it "empty" $ do
          interface <- scrod t ["default ()"]
          assertEq t interface.items [itemAt 1 1]

        t.it "basic" $ do
          interface <- scrod t ["default (Int)"]
          assertEq t interface.items [itemAt 1 1]

        t.it "multiple" $ do
          interface <- scrod t ["default (Int, Double)"]
          assertEq t interface.items [itemAt 1 1]

    t.describe "ForD" $ do
      t.describe "ForeignImport" $ do
        t.it "ccall" $ do
          interface <- scrod t ["foreign import ccall \"math.h sin\" c_sin :: Double -> Double"]
          assertEq t interface.items [itemAt 1 1]

        t.it "ccall safe" $ do
          interface <- scrod t ["foreign import ccall safe \"sleep\" c_sleep :: Int -> IO Int"]
          assertEq t interface.items [itemAt 1 1]

        t.it "ccall unsafe" $ do
          interface <- scrod t ["foreign import ccall unsafe \"getchar\" c_getchar :: IO Char"]
          assertEq t interface.items [itemAt 1 1]

        t.it "capi" $ do
          interface <- scrod t ["{-# LANGUAGE CApiFFI #-} foreign import capi \"stdio.h getchar\" c_getchar :: IO Char"]
          assertEq t interface.items [itemAt 1 26]

        t.it "stdcall (Windows)" $ do
          interface <- scrod t ["foreign import stdcall \"windows.h MessageBoxA\" c_msgbox :: Ptr () -> CString -> CString -> Int -> IO Int"]
          assertEq t interface.items [itemAt 1 1]

        t.it "prim" $ do
          interface <- scrod t ["{-# LANGUAGE GHCForeignImportPrim, MagicHash, UnliftedFFITypes #-} foreign import prim \"stg_foo\" foo# :: Int# -> Int#"]
          assertEq t interface.items [itemAt 1 68]

        t.it "wrapper" $ do
          interface <- scrod t ["foreign import ccall \"wrapper\" mkCallback :: (Int -> IO Int) -> IO (FunPtr (Int -> IO Int))"]
          assertEq t interface.items [itemAt 1 1]

        t.it "dynamic" $ do
          interface <- scrod t ["foreign import ccall \"dynamic\" callFunPtr :: FunPtr (Int -> IO Int) -> Int -> IO Int"]
          assertEq t interface.items [itemAt 1 1]

        t.it "without string" $ do
          interface <- scrod t ["foreign import ccall sin :: Double -> Double"]
          assertEq t interface.items [itemAt 1 1]

      t.describe "ForeignExport" $ do
        t.it "ccall" $ do
          interface <- scrod t ["foreign export ccall foo :: Int -> Int"]
          assertEq t interface.items [itemAt 1 1]

        t.it "ccall with name" $ do
          interface <- scrod t ["foreign export ccall \"hs_foo\" foo :: Int -> Int"]
          assertEq t interface.items [itemAt 1 1]

        t.it "stdcall (Windows)" $ do
          interface <- scrod t ["foreign export stdcall foo :: Int -> IO Int"]
          assertEq t interface.items [itemAt 1 1]

    t.describe "WarningD" $ do
      t.describe "Warnings" $ do
        t.it "one" $ do
          interface <- scrod t ["{-# WARNING x \"y\" #-}"]
          assertEq t interface.items [itemAt 1 1]

        t.it "two" $ do
          interface <- scrod t ["{-# WARNING x, y \"z\" #-}"]
          assertEq t interface.items [itemAt 1 1]

        t.it "empty list" $ do
          interface <- scrod t ["{-# WARNING x [] #-}"]
          assertEq t interface.items [itemAt 1 1]

        t.it "singleton list" $ do
          interface <- scrod t ["{-# WARNING x [\"y\"] #-}"]
          assertEq t interface.items [itemAt 1 1]

        t.it "list" $ do
          interface <- scrod t ["{-# WARNING x [\"y\", \"z\"] #-}"]
          assertEq t interface.items [itemAt 1 1]

        t.it "deprecated" $ do
          interface <- scrod t ["{-# DEPRECATED x \"y\" #-}"]
          assertEq t interface.items [itemAt 1 1]

        t.it "category" $ do
          interface <- scrod t ["{-# WARNING in \"x-foo\" bar \"qux\" #-}"]
          assertEq t interface.items [itemAt 1 1]

        t.it "explicit data" $ do
          interface <- scrod t ["{-# LANGUAGE ExplicitNamespaces #-} {-# WARNING data Foo \"bar\" #-}"]
          assertEq t interface.items [itemAt 1 37]

        t.it "explicit type" $ do
          interface <- scrod t ["{-# LANGUAGE ExplicitNamespaces #-} {-# WARNING type Foo \"bar\" #-}"]
          assertEq t interface.items [itemAt 1 37]

        t.it "instance" $ do
          interface <- scrod t ["instance {-# WARNING \"x\" #-} Cls Typ"]
          assertEq t interface.items [itemAt 1 1]

        t.it "deriving" $ do
          interface <- scrod t ["deriving instance {-# WARNING \"x\" #-} Cls Typ"]
          assertEq t interface.items [itemAt 1 1]

    t.describe "AnnD" $ do
      t.describe "HsAnnotation" $ do
        t.it "value" $ do
          interface <- scrod t ["x = 0; {-# ANN x () #-}"]
          assertEq t interface.items [itemAt 1 1, itemAt 1 8]

        t.it "type" $ do
          interface <- scrod t ["{-# ANN type T () #-}"]
          assertEq t interface.items [itemAt 1 1]

        t.it "module" $ do
          interface <- scrod t ["{-# ANN module () #-}"]
          assertEq t interface.items [itemAt 1 1]

    t.describe "RuleD" $ do
      t.describe "HsRules" $ do
        t.it "basic" $ do
          interface <- scrod t ["{-# RULES \"a\" forall x . id x = x #-}"]
          assertEq t interface.items [itemAt 1 1]

        t.it "phase" $ do
          interface <- scrod t ["{-# RULES \"b\" [2] forall x . id x = x #-}"]
          assertEq t interface.items [itemAt 1 1]

        t.it "before phase" $ do
          interface <- scrod t ["{-# RULES \"b\" [~2] forall x . id x = x #-}"]
          assertEq t interface.items [itemAt 1 1]

        t.it "disabled" $ do
          interface <- scrod t ["{-# RULES \"c\" [~] forall x . id x = x #-}"]
          assertEq t interface.items [itemAt 1 1]

        t.it "signature" $ do
          interface <- scrod t ["{-# RULES \"d\" forall (x :: Int) . id x = x #-}"]
          assertEq t interface.items [itemAt 1 1]

        t.it "semicolon" $ do
          interface <- scrod t ["{-# RULES \"e\" forall x . id x = x; \"f\" forall x . id x = x #-}"]
          assertEq t interface.items [itemAt 1 1]

    t.describe "SpliceD" $ do
      t.describe "SpliceDecl" $ do
        t.it "untyped" $ do
          interface <- scrod t ["{-# LANGUAGE TemplateHaskell #-} $(return [])"]
          assertEq t interface.items [itemAt 1 34]

        t.it "typed" $ do
          interface <- scrod t ["{-# LANGUAGE TemplateHaskell #-} $$(return [])"]
          assertEq t interface.items [itemAt 1 34]

    t.describe "DocD" $ do
      t.describe "DocCommentNamed" $ do
        t.it "works" $ do
          interface <- scrod t ["-- $foo", "-- bar"]
          assertEq t interface.items [itemAt 1 1]

      t.describe "DocGroup" $ do
        t.it "one" $ do
          interface <- scrod t ["-- * one"]
          assertEq t interface.items [itemAt 1 1]

        t.it "six" $ do
          interface <- scrod t ["-- ****** six"]
          assertEq t interface.items [itemAt 1 1]

    t.describe "RoleAnnotD" $ do
      t.describe "RoleAnnotDecl" $ do
        t.it "none" $ do
          interface <- scrod t ["{-# LANGUAGE RoleAnnotations #-} data A; type role A"]
          assertEq t interface.items [itemAt 1 34, itemAt 1 42]

        t.it "nominal" $ do
          interface <- scrod t ["{-# LANGUAGE RoleAnnotations #-} data B z; type role B nominal"]
          assertEq t interface.items [itemAt 1 34, itemAt 1 44]

        t.it "representational" $ do
          interface <- scrod t ["{-# LANGUAGE RoleAnnotations #-} data C y; type role C representational"]
          assertEq t interface.items [itemAt 1 34, itemAt 1 44]

        t.it "phantom" $ do
          interface <- scrod t ["{-# LANGUAGE RoleAnnotations #-} data D x; type role D phantom"]
          assertEq t interface.items [itemAt 1 34, itemAt 1 44]

        t.it "inferred" $ do
          interface <- scrod t ["{-# LANGUAGE RoleAnnotations #-} data E w; type role E _"]
          assertEq t interface.items [itemAt 1 34, itemAt 1 44]

        t.it "two" $ do
          interface <- scrod t ["{-# LANGUAGE RoleAnnotations #-} data F u v; type role F _ _"]
          assertEq t interface.items [itemAt 1 34, itemAt 1 46]

scrod :: (Stack.HasCallStack, Applicative m) => Test m n -> [String] -> m Interface.Interface
scrod t = expectRight t . Main.extract . unlines
