{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Scrod.Unstable.Spec where

import qualified Control.Monad as Monad
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

  -- TODO: Get all these to parse.
  t.describe "HsDecl" $ do
    t.describe "TyClD" $ do
      t.describe "FamDecl" $ do
        t.it "type family open" . Monad.void $ scrod t ["type family F a"]
        t.it "type family open multiple params" . Monad.void $ scrod t ["type family F a b"]
        t.it "type family closed" . Monad.void $ scrod t ["type family G a where { G Int = Bool; G a = Char }"]
        t.it "type family closed empty" . Monad.void $ scrod t ["type family H a where {}"]
        t.it "type family with kind sig" . Monad.void $ scrod t ["type family I a :: Type"]
        t.it "type family with result kind variable" . Monad.void $ scrod t ["type family I2 a = (r :: Type)"]
        t.it "type family with injectivity" . Monad.void $ scrod t ["type family J a = r | r -> a"]
        t.it "data family" . Monad.void $ scrod t ["data family D a"]
        t.it "data family with kind sig" . Monad.void $ scrod t ["data family E a :: Type"]
      t.describe "SynDecl" $ do
        t.it "basic" . Monad.void $ scrod t ["type S = Int"]
        t.it "with params" . Monad.void $ scrod t ["type T a = [a]"]
        t.it "with multiple params" . Monad.void $ scrod t ["type U a b = Either a b"]
        t.it "with kind annotation" . Monad.void $ scrod t ["type V (a :: Type) = Maybe a"]
        t.it "with forall" . Monad.void $ scrod t ["{-# LANGUAGE RankNTypes #-} type R = forall a. a -> a"]
        t.it "type operator" . Monad.void $ scrod t ["{-# LANGUAGE TypeOperators #-} type a + b = Either a b"]
      t.describe "DataDecl" $ do
        t.it "data basic" . Monad.void $ scrod t ["data A"]
        t.it "data with constructor" . Monad.void $ scrod t ["data B = B"]
        t.it "data with multiple constructors" . Monad.void $ scrod t ["data C = C1 | C2 | C3"]
        t.it "data with fields" . Monad.void $ scrod t ["data D = D Int Bool"]
        t.it "data with record" . Monad.void $ scrod t ["data E = E { eInt :: Int, eBool :: Bool }"]
        t.it "data with deriving" . Monad.void $ scrod t ["data F = F deriving Show"]
        t.it "data with deriving multiple" . Monad.void $ scrod t ["data G = G deriving (Show, Eq)"]
        t.it "data with deriving strategies" . Monad.void $ scrod t ["{-# LANGUAGE DerivingStrategies #-} data H = H deriving stock Show"]
        t.it "data with deriving via" . Monad.void $ scrod t ["{-# LANGUAGE DerivingVia #-} newtype I = I Int deriving Show via Int"]
        t.it "data with deriving anyclass" . Monad.void $ scrod t ["{-# LANGUAGE DeriveAnyClass, DefaultSignatures #-} class C a; data J = J deriving anyclass C"]
        t.it "data with deriving newtype" . Monad.void $ scrod t ["{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving #-} newtype K = K Int deriving newtype Num"]
        t.it "data with type params" . Monad.void $ scrod t ["data L a = L a"]
        t.it "data with phantom type" . Monad.void $ scrod t ["data M a = M"]
        t.it "data with kind sig" . Monad.void $ scrod t ["{-# LANGUAGE KindSignatures #-} data N (a :: Type) = N"]
        t.it "data with existential" . Monad.void $ scrod t ["{-# LANGUAGE ExistentialQuantification #-} data O = forall a . Show a => O a"]
        t.it "data with GADT" . Monad.void $ scrod t ["{-# LANGUAGE GADTs #-} data P a where P1 :: P Int; P2 :: P Bool"]
        t.it "data with GADT record" . Monad.void $ scrod t ["{-# LANGUAGE GADTs #-} data Q a where Q :: { qVal :: Int } -> Q Int"]
        t.it "data with strictness" . Monad.void $ scrod t ["data R = R !Int"]
        t.it "data with laziness" . Monad.void $ scrod t ["data S = S ~Int"]
        t.it "data with unpack" . Monad.void $ scrod t ["{-# LANGUAGE UnboxedTuples #-} data T = T {-# UNPACK #-} !Int"]
        t.it "data with nounpack" . Monad.void $ scrod t ["data U = U {-# NOUNPACK #-} !Int"]
        t.it "newtype basic" . Monad.void $ scrod t ["newtype V = V Int"]
        t.it "newtype with record" . Monad.void $ scrod t ["newtype W = W { unW :: Int }"]
        t.it "data with context" . Monad.void $ scrod t ["{-# LANGUAGE DatatypeContexts #-} data Eq a => X a = X a"]
        t.it "data with forall" . Monad.void $ scrod t ["{-# LANGUAGE ExplicitForAll #-} data Y = forall a . Y a"]
        t.it "data with linear field" . Monad.void $ scrod t ["{-# LANGUAGE LinearTypes #-} data Z a b = Z (a %1 -> b)"]
        t.it "data with type operator name" . Monad.void $ scrod t ["{-# LANGUAGE TypeOperators #-} data a :+: b = L a | R b"]
        t.it "data with multiple deriving clauses" . Monad.void $ scrod t ["data AA = AA deriving Show deriving Eq"]
        t.it "type data" . Monad.void $ scrod t ["{-# LANGUAGE TypeData #-} type data TBool = TTrue | TFalse"]
      t.describe "ClassDecl" $ do
        t.it "basic" . Monad.void $ scrod t ["class Cls a"]
        t.it "with method" . Monad.void $ scrod t ["class Cls a where method :: a -> a"]
        t.it "with default method" . Monad.void $ scrod t ["class Cls a where { method :: a -> a; method = id }"]
        t.it "with multiple methods" . Monad.void $ scrod t ["class Cls a where { m1 :: a; m2 :: a -> a }"]
        t.it "with superclass" . Monad.void $ scrod t ["class Eq a => Cls a"]
        t.it "with multiple superclasses" . Monad.void $ scrod t ["class (Eq a, Ord a) => Cls a"]
        t.it "with fundeps" . Monad.void $ scrod t ["{-# LANGUAGE FunctionalDependencies #-} class Cls a b | a -> b"]
        t.it "with fundeps multiple" . Monad.void $ scrod t ["{-# LANGUAGE FunctionalDependencies #-} class Cls a b c | a -> b, b -> c"]
        t.it "with associated type" . Monad.void $ scrod t ["{-# LANGUAGE TypeFamilies #-} class Cls a where type T a"]
        t.it "with associated type with kind sig" . Monad.void $ scrod t ["{-# LANGUAGE TypeFamilies #-} class Cls a where type T a :: Type -> Type"]
        t.it "with associated type default" . Monad.void $ scrod t ["{-# LANGUAGE TypeFamilies #-} class Cls a where { type T a; type T a = Int }"]
        t.it "with associated data" . Monad.void $ scrod t ["{-# LANGUAGE TypeFamilies #-} class Cls a where data D a"]
        t.it "with associated data with kind sig" . Monad.void $ scrod t ["{-# LANGUAGE TypeFamilies #-} class Cls a where data D a :: Type"]
        t.it "with quantified constraint" . Monad.void $ scrod t ["{-# LANGUAGE QuantifiedConstraints #-} class (forall x. Eq (f x)) => Cls f"]
        t.it "with default signature" . Monad.void $ scrod t ["{-# LANGUAGE DefaultSignatures #-} class Cls a where { method :: a -> a; default method :: Show a => a -> a }"]
        t.it "with minimal pragma" . Monad.void $ scrod t ["class Cls a where { m1 :: a; m2 :: a; {-# MINIMAL m1 | m2 #-} }"]
        t.it "with kind sig" . Monad.void $ scrod t ["{-# LANGUAGE KindSignatures #-} class Cls (a :: Type)"]
        t.it "no params" . Monad.void $ scrod t ["class Cls"]
        t.it "multi-param" . Monad.void $ scrod t ["{-# LANGUAGE MultiParamTypeClasses #-} class Cls a b"]
    t.describe "InstD" $ do
      t.describe "ClsInstD" $ do
        t.it "no params" . Monad.void $ scrod t ["instance Cls where"]
        t.it "basic" . Monad.void $ scrod t ["instance Cls Int where"]
        t.it "multi params" . Monad.void $ scrod t ["{-# LANGUAGE MultiParamTypeClasses #-} instance Cls Int Bool where"]
        t.it "with method" . Monad.void $ scrod t ["instance Cls Int where method = id"]
        t.it "with type application" . Monad.void $ scrod t ["instance Cls [a] where"]
        t.it "with context" . Monad.void $ scrod t ["instance Eq a => Cls [a] where"]
        t.it "with multiple contexts" . Monad.void $ scrod t ["instance (Eq a, Ord a) => Cls [a] where"]
        t.it "overlapping" . Monad.void $ scrod t ["{-# LANGUAGE FlexibleInstances #-} instance {-# OVERLAPPING #-} Cls [Int] where"]
        t.it "overlappable" . Monad.void $ scrod t ["{-# LANGUAGE FlexibleInstances #-} instance {-# OVERLAPPABLE #-} Cls [a] where"]
        t.it "overlaps" . Monad.void $ scrod t ["{-# LANGUAGE FlexibleInstances #-} instance {-# OVERLAPS #-} Cls [Int] where"]
        t.it "incoherent" . Monad.void $ scrod t ["{-# LANGUAGE FlexibleInstances #-} instance {-# INCOHERENT #-} Cls [Int] where"]
        t.it "with explicit forall" . Monad.void $ scrod t ["{-# LANGUAGE ExplicitForAll, FlexibleInstances #-} instance forall a. Cls [a] where"]
        t.it "with type application" . Monad.void $ scrod t ["{-# LANGUAGE TypeApplications #-} instance Cls @Type Int where"]
      t.describe "DataFamInstD" $ do
        t.it "basic" . Monad.void $ scrod t ["data family D a; data instance D Int = DInt"]
        t.it "with constructor" . Monad.void $ scrod t ["data family D a; data instance D Bool = DBool Bool"]
        t.it "with multiple constructors" . Monad.void $ scrod t ["data family D a; data instance D Char = DC1 | DC2 Char"]
        t.it "with record" . Monad.void $ scrod t ["data family D a; data instance D () = DUnit { dUnit :: () }"]
        t.it "newtype instance" . Monad.void $ scrod t ["data family D a; newtype instance D Float = DFloat Float"]
        t.it "with GADT" . Monad.void $ scrod t ["{-# LANGUAGE GADTs #-} data family D a; data instance D (Maybe a) where DMaybe :: a -> D (Maybe a)"]
        t.it "with forall" . Monad.void $ scrod t ["data family D a; data instance forall a . D [a] = DList [a]"]
        t.it "with deriving" . Monad.void $ scrod t ["data family D a; data instance D Word = DWord Word deriving Show"]
      t.describe "TyFamInstD" $ do
        t.it "basic" . Monad.void $ scrod t ["type family F a; type instance F Int = Bool"]
        t.it "with type var" . Monad.void $ scrod t ["type family F a; type instance F [a] = a"]
        t.it "with nested" . Monad.void $ scrod t ["type family F a; type instance F (Maybe a) = Either () a"]
        t.it "with forall" . Monad.void $ scrod t ["type family F a; type instance forall a. F [a] = a"]
    t.describe "DerivD" $ do
      t.describe "DerivDecl" $ do
        t.it "basic" . Monad.void $ scrod t ["data A = A; deriving instance Show A"]
        t.it "with context" . Monad.void $ scrod t ["data B a = B a; deriving instance Show a => Show (B a)"]
        t.it "stock" . Monad.void $ scrod t ["{-# LANGUAGE DerivingStrategies #-} data C = C; deriving stock instance Show C"]
        t.it "newtype" . Monad.void $ scrod t ["{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving #-} newtype D = D Int; deriving newtype instance Num D"]
        t.it "anyclass" . Monad.void $ scrod t ["{-# LANGUAGE DerivingStrategies, DeriveAnyClass #-} class Cls a; data E = E; deriving anyclass instance Cls E"]
        t.it "via" . Monad.void $ scrod t ["{-# LANGUAGE DerivingStrategies, DerivingVia, StandaloneDeriving #-} newtype F = F Int; deriving via Int instance Show F"]
    t.describe "ValD" $ do
      t.describe "FunBind" $ do
        t.it "basic" . Monad.void $ scrod t ["f x = x"]
        t.it "with multiple args" . Monad.void $ scrod t ["f x y z = x"]
        t.it "with pattern matching" . Monad.void $ scrod t ["f [] = 0; f (x:xs) = 1 + f xs"]
        t.it "with guards" . Monad.void $ scrod t ["f x | x > 0 = 1 | otherwise = 0"]
        t.it "with where" . Monad.void $ scrod t ["f x = y where y = x + 1"]
        t.it "with let" . Monad.void $ scrod t ["f x = let y = x + 1 in y"]
        t.it "operator" . Monad.void $ scrod t ["x +++ y = x + y"]
        t.it "operator prefix" . Monad.void $ scrod t ["(+++) x y = x + y"]
        t.it "infix" . Monad.void $ scrod t [" x `plus` y = x + y "]
        t.it "with view pattern" . Monad.void $ scrod t ["{-# LANGUAGE ViewPatterns #-} f (show -> s) = s"]
        t.it "with pattern guard" . Monad.void $ scrod t ["f x | Just y <- g x = y | otherwise = x"]
        t.it "with type abstraction" . Monad.void $ scrod t ["{-# LANGUAGE TypeAbstractions #-} f @a (x :: a) = x"]
      t.describe "PatBind" $ do
        t.it "simple" . Monad.void $ scrod t ["x = 1"]
        t.it "tuple" . Monad.void $ scrod t ["(a, b) = (1, 2)"]
        t.it "list" . Monad.void $ scrod t ["[x, y] = [1, 2]"]
        t.it "cons" . Monad.void $ scrod t ["(h:t) = [1, 2, 3]"]
        t.it "record" . Monad.void $ scrod t ["data T = C { f :: Int }; C { f = x } = C 1"]
        t.it "as pattern" . Monad.void $ scrod t ["all@(x:xs) = [1, 2, 3]"]
        t.it "wildcard" . Monad.void $ scrod t ["_ = undefined"]
        t.it "lazy" . Monad.void $ scrod t ["~(a, b) = undefined"]
        t.it "bang" . Monad.void $ scrod t ["{-# LANGUAGE BangPatterns #-} !x = 1"]
        t.it "with type signature" . Monad.void $ scrod t ["{-# LANGUAGE ScopedTypeVariables #-} (x :: Int) = 1"]
        t.it "with view pattern" . Monad.void $ scrod t ["{-# LANGUAGE ViewPatterns #-} (reverse -> xs) = [1,2,3]"]
        t.it "or pattern" . Monad.void $ scrod t ["{-# LANGUAGE OrPatterns #-} (Left x; Right x) = undefined"]
      t.describe "PatSynBind" $ do
        t.it "unidirectional" . Monad.void $ scrod t ["{-# LANGUAGE PatternSynonyms #-} pattern P x = Just x"]
        t.it "bidirectional implicit" . Monad.void $ scrod t ["{-# LANGUAGE PatternSynonyms #-} pattern Q x = (x, x)"]
        t.it "bidirectional explicit" . Monad.void $ scrod t ["{-# LANGUAGE PatternSynonyms #-} pattern R x <- Just x where R x = Just x"]
        t.it "view pattern" . Monad.void $ scrod t ["{-# LANGUAGE PatternSynonyms, ViewPatterns #-} pattern S x <- (show -> x)"]
        t.it "record" . Monad.void $ scrod t ["{-# LANGUAGE PatternSynonyms #-} pattern T { tField } = Just tField"]
        t.it "prefix" . Monad.void $ scrod t ["{-# LANGUAGE PatternSynonyms #-} pattern P x = Just x"]
        t.it "infix" . Monad.void $ scrod t ["{-# LANGUAGE PatternSynonyms #-} pattern x :+: y = (x, y)"]
    t.describe "SigD" $ do
      t.describe "TypeSig" $ do
        t.it "basic" . Monad.void $ scrod t ["f :: Int"]
        t.it "with args" . Monad.void $ scrod t ["f :: Int -> Bool"]
        t.it "with implicit forall" . Monad.void $ scrod t ["f :: a -> a"]
        t.it "with explicit forall visible" . Monad.void $ scrod t ["f :: forall a . a -> a"]
        t.it "with explicit forall invisible" . Monad.void $ scrod t ["f :: forall {a} . a -> a"]
        t.it "with context" . Monad.void $ scrod t ["f :: Eq a => a -> Bool"]
        t.it "with multiple names" . Monad.void $ scrod t ["f, g :: Int"]
        t.it "with kind sig" . Monad.void $ scrod t ["{-# LANGUAGE KindSignatures #-} f :: forall (a :: Type) . a -> a"]
        t.it "with linear arrow" . Monad.void $ scrod t ["{-# LANGUAGE LinearTypes #-} f :: a %1 -> b"]
        t.it "with multiplicity poly" . Monad.void $ scrod t ["{-# LANGUAGE LinearTypes #-} f :: a %m -> b"]
        t.it "with visible forall" . Monad.void $ scrod t ["{-# LANGUAGE RequiredTypeArguments #-} f :: forall a -> a -> a"]
      t.describe "PatSynSig" $ do
        t.it "basic" . Monad.void $ scrod t ["{-# LANGUAGE PatternSynonyms #-} pattern P :: Int -> Maybe Int"]
        t.it "with forall provided" . Monad.void $ scrod t ["{-# LANGUAGE PatternSynonyms #-} pattern Q :: forall a . a -> Maybe a"]
        t.it "with forall required" . Monad.void $ scrod t ["{-# LANGUAGE PatternSynonyms, ExplicitForAll #-} pattern R :: forall a . Show a => a -> Maybe a"]
        t.it "bidirectional" . Monad.void $ scrod t ["{-# LANGUAGE PatternSynonyms #-} pattern S :: forall a . () => (Eq a) => a -> Maybe a"]
      t.describe "ClassOpSig" $ do
        t.it "basic" . Monad.void $ scrod t ["class C a where op :: a -> a"]
        t.it "default" . Monad.void $ scrod t ["class C a where { op :: a -> a; default op :: a -> a }"]
        t.it "multiple" . Monad.void $ scrod t ["class C a where op1, op2 :: a -> a"]
      t.describe "FixSig" $ do
        t.it "infixl" . Monad.void $ scrod t ["infixl 6 +++"]
        t.it "infixr" . Monad.void $ scrod t ["infixr 5 +++"]
        t.it "infix" . Monad.void $ scrod t ["infix 4 +++"]
        t.it "non-operator" . Monad.void $ scrod t [" infix 5 `T` "]
        t.it "multiple" . Monad.void $ scrod t ["infixl 6 +++, ---"]
        t.it "precedence 0" . Monad.void $ scrod t ["infixl 0 +++"]
        t.it "precedence 9" . Monad.void $ scrod t ["infixr 9 +++"]
      t.describe "InlineSig" $ do
        t.it "inline" . Monad.void $ scrod t ["{-# INLINE f #-}"]
        t.it "inline operator" . Monad.void $ scrod t ["{-# INLINE (+++) #-}"]
        t.it "conlike" . Monad.void $ scrod t ["{-# INLINE CONLIKE f #-}"]
        t.it "noinline" . Monad.void $ scrod t ["{-# NOINLINE f #-}"]
        t.it "inlinable" . Monad.void $ scrod t ["{-# INLINABLE f #-}"]
        t.it "inline phase" . Monad.void $ scrod t ["{-# INLINE [2] f #-}"]
        t.it "inline before phase" . Monad.void $ scrod t ["{-# INLINE [~2] f #-}"]
        t.it "opaque" . Monad.void $ scrod t ["{-# OPAQUE f #-}"]
      t.describe "SpecSig" $ do
        t.it "basic" . Monad.void $ scrod t ["{-# SPECIALIZE f :: Int -> Int #-}"]
        t.it "uk" . Monad.void $ scrod t ["{-# SPECIALISE f :: Int -> Int #-}"]
        t.it "with inline" . Monad.void $ scrod t ["{-# SPECIALIZE INLINE f :: Int -> Int #-}"]
        t.it "with noinline" . Monad.void $ scrod t ["{-# SPECIALIZE NOINLINE f :: Int -> Int #-}"]
        t.it "with phase" . Monad.void $ scrod t ["{-# SPECIALIZE [1] f :: Int -> Int #-}"]
        t.it "with before phase" . Monad.void $ scrod t ["{-# SPECIALIZE [~1] f :: Int -> Int #-}"]
      t.describe "SpecInstSig" $ do
        t.it "basic" . Monad.void $ scrod t ["instance Cls [a] where {-# SPECIALIZE instance Cls [Int] #-}"]
        t.it "with context" . Monad.void $ scrod t ["{-# SPECIALIZE instance Eq a => Cls [a] #-}"]
      t.describe "MinimalSig" $ do
        t.it "single" . Monad.void $ scrod t ["class C a where { m :: a; {-# MINIMAL m #-} }"]
        t.it "or" . Monad.void $ scrod t ["class C a where { m1, m2 :: a; {-# MINIMAL m1 | m2 #-} }"]
        t.it "and" . Monad.void $ scrod t ["class C a where { m1, m2 :: a; {-# MINIMAL m1, m2 #-} }"]
        t.it "nested" . Monad.void $ scrod t ["class C a where { m1, m2, m3 :: a; {-# MINIMAL (m1, m2) | m3 #-} }"]
        t.it "empty" . Monad.void $ scrod t ["class C a where { m :: a; m = undefined; {-# MINIMAL #-} }"]
      t.describe "SCCFunSig" $ do
        t.it "basic" . Monad.void $ scrod t ["{-# SCC f #-}"]
        t.it "string" . Monad.void $ scrod t ["{-# SCC \"f\" #-}"]
        t.it "with string" . Monad.void $ scrod t ["{-# SCC f \"cost-centre-name\" #-}"]
      t.describe "CompleteMatchSig" $ do
        t.it "basic" . Monad.void $ scrod t ["{-# COMPLETE P #-}"]
        t.it "multiple" . Monad.void $ scrod t ["{-# COMPLETE P, Q #-}"]
        t.it "with type" . Monad.void $ scrod t ["{-# COMPLETE P :: Maybe #-}"]
        t.it "with type multiple" . Monad.void $ scrod t ["{-# COMPLETE P, Q :: Either #-}"]
    t.describe "KindSigD" $ do
      t.describe "StandaloneKindSig" $ do
        t.it "basic" . Monad.void $ scrod t ["{-# LANGUAGE StandaloneKindSignatures #-} type T :: Type; data T"]
        t.it "with arrow" . Monad.void $ scrod t ["{-# LANGUAGE StandaloneKindSignatures #-} type T :: Type -> Type; data T a"]
        t.it "with constraint" . Monad.void $ scrod t ["{-# LANGUAGE StandaloneKindSignatures #-} type C :: Type -> Constraint; class C a"]
        t.it "with poly kind" . Monad.void $ scrod t ["{-# LANGUAGE StandaloneKindSignatures, PolyKinds #-} type T :: forall k . k -> Type; data T a"]
        t.it "for type family" . Monad.void $ scrod t ["{-# LANGUAGE StandaloneKindSignatures, TypeFamilies #-} type F :: Type -> Type; type family F a"]
        t.it "for type synonym" . Monad.void $ scrod t ["{-# LANGUAGE StandaloneKindSignatures #-} type S :: Type -> Type; type S a = [a]"]
        t.it "for data family" . Monad.void $ scrod t ["{-# LANGUAGE StandaloneKindSignatures, TypeFamilies #-} type D :: Type -> Type; data family D a"]
    t.describe "DefD" $ do
      t.describe "DefaultDecl" $ do
        t.it "empty" . Monad.void $ scrod t ["default ()"]
        t.it "basic" . Monad.void $ scrod t ["default (Int)"]
        t.it "multiple" . Monad.void $ scrod t ["default (Int, Double)"]
        t.it "named" . Monad.void $ scrod t ["{-# LANGUAGE NamedDefaults #-} default Num (Int, Double)"]
    t.describe "ForD" $ do
      t.describe "ForeignImport" $ do
        t.it "ccall" . Monad.void $ scrod t ["foreign import ccall \"math.h sin\" c_sin :: Double -> Double"]
        t.it "ccall safe" . Monad.void $ scrod t ["foreign import ccall safe \"sleep\" c_sleep :: Int -> IO Int"]
        t.it "ccall unsafe" . Monad.void $ scrod t ["foreign import ccall unsafe \"getchar\" c_getchar :: IO Char"]
        t.it "ccall interruptible" . Monad.void $ scrod t ["foreign import ccall interruptible \"longop\" c_longop :: IO ()"]
        t.it "capi" . Monad.void $ scrod t ["{-# LANGUAGE CApiFFI #-} foreign import capi \"stdio.h getchar\" c_getchar :: IO Char"]
        t.it "stdcall (Windows)" . Monad.void $ scrod t ["foreign import stdcall \"windows.h MessageBoxA\" c_msgbox :: Ptr () -> CString -> CString -> Int -> IO Int"]
        t.it "prim" . Monad.void $ scrod t ["{-# LANGUAGE GHCForeignImportPrim, MagicHash, UnliftedFFITypes #-} foreign import prim \"stg_foo\" foo# :: Int# -> Int#"]
        t.it "javascript (GHC JS)" . Monad.void $ scrod t ["{-# LANGUAGE JavaScriptFFI #-} foreign import javascript \"console.log\" js_log :: JSVal -> IO ()"]
        t.it "wrapper" . Monad.void $ scrod t ["foreign import ccall \"wrapper\" mkCallback :: (Int -> IO Int) -> IO (FunPtr (Int -> IO Int))"]
        t.it "dynamic" . Monad.void $ scrod t ["foreign import ccall \"dynamic\" callFunPtr :: FunPtr (Int -> IO Int) -> Int -> IO Int"]
        t.it "without string" . Monad.void $ scrod t ["foreign import ccall sin :: Double -> Double"]
      t.describe "ForeignExport" $ do
        t.it "ccall" . Monad.void $ scrod t ["foreign export ccall foo :: Int -> Int"]
        t.it "ccall with name" . Monad.void $ scrod t ["foreign export ccall \"hs_foo\" foo :: Int -> Int"]
        t.it "stdcall (Windows)" . Monad.void $ scrod t ["foreign export stdcall foo :: Int -> IO Int"]
    t.describe "WarningD" $ do
      t.describe "Warnings" $ do
        t.it "one" . Monad.void $ scrod t ["{-# WARNING x \"y\" #-}"]
        t.it "two" . Monad.void $ scrod t ["{-# WARNING x, y \"z\" #-}"]
        t.it "empty list" . Monad.void $ scrod t ["{-# WARNING x [] #-}"]
        t.it "singleton list" . Monad.void $ scrod t ["{-# WARNING x [\"y\"] #-}"]
        t.it "list" . Monad.void $ scrod t ["{-# WARNING x [\"y\", \"z\"] #-}"]
        t.it "deprecated" . Monad.void $ scrod t ["{-# DEPRECATED x \"y\" #-}"]
        t.it "category" . Monad.void $ scrod t ["{-# WARNING in \"x-foo\" bar \"qux\" #-}"]
        t.it "explicit data" . Monad.void $ scrod t ["{-# LANGUAGE ExplicitNamespaces #-} {-# WARNING data Foo \"bar\" #-}"]
        t.it "explicit type" . Monad.void $ scrod t ["{-# LANGUAGE ExplicitNamespaces #-} {-# WARNING type Foo \"bar\" #-}"]
        t.it "instance" . Monad.void $ scrod t ["instance {-# WARNING \"x\" #-} Cls Typ"]
        t.it "deriving" . Monad.void $ scrod t ["deriving instance {-# WARNING \"x\" #-} Cls Typ"]
    t.describe "AnnD" $ do
      t.describe "HsAnnotation" $ do
        t.it "value" . Monad.void $ scrod t ["x = 0; {-# ANN x () #-}"]
        t.it "type" . Monad.void $ scrod t ["{-# ANN type T () #-}"]
        t.it "module" . Monad.void $ scrod t ["{-# ANN module () #-}"]
    t.describe "RuleD" $ do
      t.describe "HsRules" $ do
        t.it "basic" . Monad.void $ scrod t ["{-# RULES \"a\" forall x . id x = x #-}"]
        t.it "phase" . Monad.void $ scrod t ["{-# RULES \"b\" [2] forall x . id x = x #-}"]
        t.it "before phase" . Monad.void $ scrod t ["{-# RULES \"b\" [~2] forall x . id x = x #-}"]
        t.it "disabled" . Monad.void $ scrod t ["{-# RULES \"c\" [~] forall x . id x = x #-}"]
        t.it "signature" . Monad.void $ scrod t ["{-# RULES \"d\" forall (x :: Int) . id x = x #-}"]
        t.it "type binders" . Monad.void $ scrod t ["{-# RULES \"d2\" forall (type a) (x :: a) . id x = x #-}"]
        t.it "semicolon" . Monad.void $ scrod t ["{-# RULES \"e\" forall x . id x = x; \"f\" forall x . id x = x #-}"]
        t.it "newline" . Monad.void $ scrod t ["{-# RULES \"g\"", " forall x . id x = x", " \"h\" forall x . id x = x", " #-}"]
    t.describe "SpliceD" $ do
      t.describe "SpliceDecl" $ do
        t.it "untyped" . Monad.void $ scrod t ["{-# LANGUAGE TemplateHaskell #-} $(return [])"]
        t.it "typed" . Monad.void $ scrod t ["{-# LANGUAGE TemplateHaskell #-} $$(return [])"]
    t.describe "DocD" $ do
      t.describe "DocCommentNamed" $ do
        t.it "works" . Monad.void $ scrod t ["-- $foo", "-- bar"]
      t.describe "DocGroup" $ do
        t.it "one" . Monad.void $ scrod t ["-- * one"]
        t.it "six" . Monad.void $ scrod t ["-- ****** six"]
    t.describe "RoleAnnotD" $ do
      t.describe "RoleAnnotDecl" $ do
        t.it "none" . Monad.void $ scrod t ["{-# LANGUAGE RoleAnnotations #-} data A; type role A"]
        t.it "nominal" . Monad.void $ scrod t ["{-# LANGUAGE RoleAnnotations #-} data B z; type role B nominal"]
        t.it "representational" . Monad.void $ scrod t ["{-# LANGUAGE RoleAnnotations #-} data C y; type role C representational"]
        t.it "phantom" . Monad.void $ scrod t ["{-# LANGUAGE RoleAnnotations #-} data D x; type role D phantom"]
        t.it "inferred" . Monad.void $ scrod t ["{-# LANGUAGE RoleAnnotations #-} data E w; type role E _"]
        t.it "two" . Monad.void $ scrod t ["{-# LANGUAGE RoleAnnotations #-} data F u v; type role F _ _"]

scrod :: (Stack.HasCallStack, Applicative m) => Test m n -> [String] -> m Interface.Interface
scrod t = expectRight t . Main.extract . unlines
