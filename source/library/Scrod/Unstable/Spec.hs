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

scrod :: (Stack.HasCallStack, Applicative m) => Test m n -> [String] -> m Interface.Interface
scrod t = expectRight t . Main.extract . unlines
