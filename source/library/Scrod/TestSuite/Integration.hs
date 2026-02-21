{-# LANGUAGE MultilineStrings #-}
{-# OPTIONS_GHC -O0 #-}

module Scrod.TestSuite.Integration where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Data.List as List
import qualified GHC.Stack as Stack
import qualified Scrod.Executable.Main as Main
import qualified Scrod.Extra.Builder as Builder
import qualified Scrod.Extra.Parsec as Parsec
import qualified Scrod.Json.Value as Json
import qualified Scrod.JsonPointer.Evaluate as Pointer
import qualified Scrod.JsonPointer.Pointer as Pointer
import qualified Scrod.Spec as Spec

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "integration" $ do
  Spec.it s "works with empty input" $ do
    check
      s
      ""
      [ ("/extensions", "{}"),
        ("/documentation/type", "\"Empty\""),
        ("/imports", "[]"),
        ("/signature", "false"),
        ("/items", "[]")
      ]

  Spec.describe s "$schema" $ do
    Spec.it s "is present" $ do
      check s "" [("/$schema", "\"https://scrod.fyi/schema.json\"")]

  Spec.describe s "version" $ do
    Spec.it s "works" $ do
      -- Note that we don't want this test to be too specific, otherwise we'd
      -- have to update it every time we change the version number. So instead
      -- we just check the first component.
      check s "" [("/version/0", "0")]

  Spec.describe s "language" $ do
    Spec.it s "defaults to absent" $ do
      check s "" [("/language", "")]

    Spec.it s "works with a language" $ do
      check s "{-# language Haskell98 #-}" [("/language", "\"Haskell98\"")]

    Spec.it s "works with GHC2024" $ do
      check s "{-# language GHC2024 #-}" [("/language", "\"GHC2024\"")]

    Spec.it s "picks the last one" $ do
      check s "{-# language Haskell98, Haskell2010 #-}" [("/language", "\"Haskell2010\"")]

  Spec.describe s "extensions" $ do
    Spec.it s "defaults to empty object" $ do
      check s "" [("/extensions", "{}")]

    Spec.it s "works with an enabled extension" $ do
      check s "{-# language CPP #-}" [("/extensions/Cpp", "true")]

    Spec.it s "works with a disabled extension" $ do
      check s "{-# language NoCPP #-}" [("/extensions/Cpp", "false")]

    Spec.it s "works with on then off" $ do
      check s "{-# language CPP, NoCPP #-}" [("/extensions/Cpp", "false")]

    Spec.it s "works with off then on" $ do
      check s "{-# language NoCPP, CPP #-}" [("/extensions/Cpp", "true")]

    Spec.it s "works with ghc options" $ do
      check s "{-# options_ghc -XCPP #-}" [("/extensions/Cpp", "true")]

    Spec.it s "works with optimization flags" $ do
      check s "{-# options_ghc -O #-}" [("/items", "[]")]

    Spec.it s "works with two extensions in one pragma" $ do
      check
        s
        "{-# language CPP, DeriveGeneric #-}"
        [ ("/extensions/Cpp", "true"),
          ("/extensions/DeriveGeneric", "true")
        ]

    Spec.it s "works with two extensions in separate pragmas" $ do
      check
        s
        "{-# language CPP #-} {-# language DeriveGeneric #-}"
        [ ("/extensions/Cpp", "true"),
          ("/extensions/DeriveGeneric", "true")
        ]

    Spec.it s "also enables implied extensions" $ do
      check
        s
        "{-# language RankNTypes #-}"
        [ ("/extensions/ExplicitForAll", "true"),
          ("/extensions/RankNTypes", "true")
        ]

    Spec.it s "works with cabal script header" $ do
      check
        s
        """
        {- cabal:
        default-extensions: CPP
        -}
        """
        [("/extensions/Cpp", "true")]

    Spec.it s "works with cabal script header and pragma" $ do
      check
        s
        """
        {- cabal:
        default-extensions: CPP
        -}
        {-# language DeriveGeneric #-}
        """
        [ ("/extensions/Cpp", "true"),
          ("/extensions/DeriveGeneric", "true")
        ]

    Spec.it s "works with cabal script header with shebang" $ do
      check
        s
        """
        #!/usr/bin/env cabal
        {- cabal:
        default-extensions: CPP
        -}
        """
        [("/extensions/Cpp", "true")]

    Spec.it s "works with --ghc-option extension" $ do
      checkWith
        s
        ["--ghc-option", "-XOverloadedStrings"]
        ""
        [("/extensions/OverloadedStrings", "true")]

    Spec.it s "works with --ghc-option language" $ do
      checkWith
        s
        ["--ghc-option", "-XHaskell2010"]
        ""
        [("/language", "\"Haskell2010\"")]

    Spec.it s "source pragmas override --ghc-option" $ do
      checkWith
        s
        ["--ghc-option", "-XOverloadedStrings"]
        "{-# language NoOverloadedStrings #-}"
        [("/extensions/OverloadedStrings", "false")]

  Spec.describe s "documentation" $ do
    Spec.it s "defaults to Empty" $ do
      check s "" [("/documentation/type", "\"Empty\"")]

    Spec.it s "works with a string" $ do
      check
        s
        """
        -- | x
        module M where
        """
        [ ("/documentation/type", "\"Paragraph\""),
          ("/documentation/value/type", "\"String\""),
          ("/documentation/value/value", "\"x\"")
        ]

    Spec.it s "works with an identifier" $ do
      check
        s
        """
        -- | 'x'
        module M where
        """
        [ ("/documentation/type", "\"Paragraph\""),
          ("/documentation/value/type", "\"Identifier\""),
          ("/documentation/value/value/value", "\"x\"")
        ]

    Spec.it s "strips module qualifier from identifier" $ do
      check
        s
        """
        -- | 'A.b'
        module M where
        """
        [ ("/documentation/type", "\"Paragraph\""),
          ("/documentation/value/type", "\"Identifier\""),
          ("/documentation/value/value/value", "\"b\"")
        ]

    Spec.it s "works with a value identifier" $ do
      check
        s
        """
        -- | v'x'
        module M where
        """
        [ ("/documentation/type", "\"Paragraph\""),
          ("/documentation/value/type", "\"Identifier\""),
          ("/documentation/value/value/namespace/type", "\"Value\""),
          ("/documentation/value/value/value", "\"x\"")
        ]

    Spec.it s "works with a type identifier" $ do
      check
        s
        """
        -- | t'x'
        module M where
        """
        [ ("/documentation/type", "\"Paragraph\""),
          ("/documentation/value/type", "\"Identifier\""),
          ("/documentation/value/value/namespace/type", "\"Type\""),
          ("/documentation/value/value/value", "\"x\"")
        ]

    Spec.it s "works with a module" $ do
      check
        s
        """
        -- | "X"
        module M where
        """
        [ ("/documentation/type", "\"Paragraph\""),
          ("/documentation/value/type", "\"Module\""),
          ("/documentation/value/value/name", "\"X\"")
        ]

    Spec.it s "works with a labeled module" $ do
      check
        s
        """
        -- | [label]("X")
        module M where
        """
        [ ("/documentation/type", "\"Paragraph\""),
          ("/documentation/value/type", "\"Module\""),
          ("/documentation/value/value/name", "\"X\""),
          ("/documentation/value/value/label/type", "\"String\""),
          ("/documentation/value/value/label/value", "\"label\"")
        ]

    Spec.it s "works with emphasis" $ do
      check
        s
        """
        -- | /x/
        module M where
        """
        [ ("/documentation/type", "\"Paragraph\""),
          ("/documentation/value/type", "\"Emphasis\""),
          ("/documentation/value/value/type", "\"String\""),
          ("/documentation/value/value/value", "\"x\"")
        ]

    Spec.it s "works with monospaced text" $ do
      -- Note that this can't use `@x@` by itself because that would be a code
      -- block.
      check
        s
        """
        -- | x @y@
        module M where
        """
        [ ("/documentation/type", "\"Paragraph\""),
          ("/documentation/value/type", "\"Append\""),
          ("/documentation/value/value/0/type", "\"String\""),
          ("/documentation/value/value/0/value", "\"x \""),
          ("/documentation/value/value/1/type", "\"Monospaced\""),
          ("/documentation/value/value/1/value/type", "\"String\""),
          ("/documentation/value/value/1/value/value", "\"y\"")
        ]

    Spec.it s "works with bold text" $ do
      check
        s
        """
        -- | __x__
        module M where
        """
        [ ("/documentation/type", "\"Paragraph\""),
          ("/documentation/value/type", "\"Bold\""),
          ("/documentation/value/value/type", "\"String\""),
          ("/documentation/value/value/value", "\"x\"")
        ]

    Spec.it s "works with an unordered list using dash" $ do
      check
        s
        """
        -- | - x
        module M where
        """
        [ ("/documentation/type", "\"UnorderedList\""),
          ("/documentation/value/0/type", "\"Paragraph\""),
          ("/documentation/value/0/value/type", "\"String\""),
          ("/documentation/value/0/value/value", "\"x\"")
        ]

    Spec.it s "works with an unordered list using asterisk" $ do
      check
        s
        """
        -- | * x
        module M where
        """
        [ ("/documentation/type", "\"UnorderedList\""),
          ("/documentation/value/0/type", "\"Paragraph\""),
          ("/documentation/value/0/value/type", "\"String\""),
          ("/documentation/value/0/value/value", "\"x\"")
        ]

    Spec.it s "works with an ordered list using period" $ do
      check
        s
        """
        -- | 1. x
        module M where
        """
        [ ("/documentation/type", "\"OrderedList\""),
          ("/documentation/value/0/index", "1"),
          ("/documentation/value/0/item/type", "\"Paragraph\""),
          ("/documentation/value/0/item/value/type", "\"String\""),
          ("/documentation/value/0/item/value/value", "\"x\"")
        ]

    Spec.it s "works with an ordered list using parens" $ do
      check
        s
        """
        -- | (1) x
        module M where
        """
        [ ("/documentation/type", "\"OrderedList\""),
          ("/documentation/value/0/index", "1"),
          ("/documentation/value/0/item/type", "\"Paragraph\""),
          ("/documentation/value/0/item/value/type", "\"String\""),
          ("/documentation/value/0/item/value/value", "\"x\"")
        ]

    Spec.it s "works with an ordered list starting at 2" $ do
      check
        s
        """
        -- | 2. x
        module M where
        """
        [ ("/documentation/type", "\"OrderedList\""),
          ("/documentation/value/0/index", "2"),
          ("/documentation/value/0/item/type", "\"Paragraph\""),
          ("/documentation/value/0/item/value/type", "\"String\""),
          ("/documentation/value/0/item/value/value", "\"x\"")
        ]

    Spec.it s "works with a definition list" $ do
      check
        s
        """
        -- | [x]: y
        module M where
        """
        [ ("/documentation/type", "\"DefList\""),
          ("/documentation/value/0/term/type", "\"String\""),
          ("/documentation/value/0/term/value", "\"x\""),
          ("/documentation/value/0/definition/type", "\"String\""),
          ("/documentation/value/0/definition/value", "\"y\"")
        ]

    Spec.it s "works with a code block" $ do
      check
        s
        """
        -- | @x@
        module M where
        """
        [ ("/documentation/type", "\"CodeBlock\""),
          ("/documentation/value/type", "\"String\""),
          ("/documentation/value/value", "\"x\"")
        ]

    Spec.it s "works with a hyperlink" $ do
      check
        s
        """
        -- | <http://example>
        module M where
        """
        [ ("/documentation/type", "\"Paragraph\""),
          ("/documentation/value/type", "\"Hyperlink\""),
          ("/documentation/value/value/url", "\"http://example\"")
        ]

    Spec.it s "works with a labeled hyperlink" $ do
      check
        s
        """
        -- | [http://example](x)
        module M where
        """
        [ ("/documentation/type", "\"Paragraph\""),
          ("/documentation/value/type", "\"Hyperlink\""),
          ("/documentation/value/value/url", "\"x\""),
          ("/documentation/value/value/label/type", "\"Hyperlink\""),
          ("/documentation/value/value/label/value/url", "\"http://example\"")
        ]

    Spec.it s "works with a picture" $ do
      check
        s
        """
        -- | ![http://example](x)
        module M where
        """
        [ ("/documentation/type", "\"Paragraph\""),
          ("/documentation/value/type", "\"Pic\""),
          ("/documentation/value/value/uri", "\"x\""),
          ("/documentation/value/value/title", "\"http://example\"")
        ]

    Spec.it s "works with inline math" $ do
      check
        s
        """
        -- | \\(x\\)
        module M where
        """
        [ ("/documentation/type", "\"Paragraph\""),
          ("/documentation/value/type", "\"MathInline\""),
          ("/documentation/value/value", "\"x\"")
        ]

    Spec.it s "works with display math" $ do
      check
        s
        """
        -- | \\[x\\]
        module M where
        """
        [ ("/documentation/type", "\"Paragraph\""),
          ("/documentation/value/type", "\"MathDisplay\""),
          ("/documentation/value/value", "\"x\"")
        ]

    Spec.it s "works with an anchor" $ do
      check
        s
        """
        -- | #x#
        module M where
        """
        [ ("/documentation/type", "\"Paragraph\""),
          ("/documentation/value/type", "\"AName\""),
          ("/documentation/value/value", "\"x\"")
        ]

    Spec.it s "works with a property" $ do
      check
        s
        """
        -- | prop> x
        module M where
        """
        [ ("/documentation/type", "\"Property\""),
          ("/documentation/value", "\"x\"")
        ]

    Spec.it s "works with an example" $ do
      check
        s
        """
        -- | >>> x
        module M where
        """
        [ ("/documentation/type", "\"Examples\""),
          ("/documentation/value/0/expression", "\"x\""),
          ("/documentation/value/0/result", "[]")
        ]

    Spec.it s "works with an example with a result" $ do
      check
        s
        """
        -- | >>> x
        -- y
        module M where
        """
        [ ("/documentation/type", "\"Examples\""),
          ("/documentation/value/0/expression", "\"x\""),
          ("/documentation/value/0/result/0", "\"y\"")
        ]

    Spec.it s "works with an example with a blank line" $ do
      check
        s
        """
        -- | >>> x
        -- y
        -- <BLANKLINE>
        -- z
        module M where
        """
        [ ("/documentation/type", "\"Examples\""),
          ("/documentation/value/0/expression", "\"x\""),
          ("/documentation/value/0/result/0", "\"y\""),
          ("/documentation/value/0/result/1", "\"\""),
          ("/documentation/value/0/result/2", "\"z\"")
        ]

    Spec.it s "works with a header" $ do
      check
        s
        """
        -- | = x
        module M where
        """
        [ ("/documentation/type", "\"Header\""),
          ("/documentation/value/level", "1"),
          ("/documentation/value/title/type", "\"String\""),
          ("/documentation/value/title/value", "\"x\"")
        ]

    Spec.it s "works with a level 2 header" $ do
      check
        s
        """
        -- | == x
        module M where
        """
        [ ("/documentation/type", "\"Header\""),
          ("/documentation/value/level", "2")
        ]

    Spec.it s "works with a level 3 header" $ do
      check
        s
        """
        -- | === x
        module M where
        """
        [ ("/documentation/type", "\"Header\""),
          ("/documentation/value/level", "3")
        ]

    Spec.it s "works with a table" $ do
      check
        s
        """
        -- |
        -- +---+---+---+
        -- | a | b | c |
        -- +===+===+===+
        -- | d     | e |
        -- +---+---+   |
        -- | f | g |   |
        -- +---+---+---+
        module M where
        """
        [ ("/documentation/type", "\"Table\""),
          ("/documentation/value/headerRows/0/0/colspan", "1"),
          ("/documentation/value/headerRows/0/0/rowspan", "1"),
          ("/documentation/value/headerRows/0/0/contents/type", "\"String\""),
          ("/documentation/value/headerRows/0/0/contents/value", "\"a\""),
          ("/documentation/value/headerRows/0/1/colspan", "1"),
          ("/documentation/value/headerRows/0/1/rowspan", "1"),
          ("/documentation/value/headerRows/0/1/contents/type", "\"String\""),
          ("/documentation/value/headerRows/0/1/contents/value", "\"b\""),
          ("/documentation/value/headerRows/0/2/colspan", "1"),
          ("/documentation/value/headerRows/0/2/rowspan", "1"),
          ("/documentation/value/headerRows/0/2/contents/type", "\"String\""),
          ("/documentation/value/headerRows/0/2/contents/value", "\"c\""),
          ("/documentation/value/bodyRows/0/0/colspan", "2"),
          ("/documentation/value/bodyRows/0/0/rowspan", "1"),
          ("/documentation/value/bodyRows/0/0/contents/type", "\"String\""),
          ("/documentation/value/bodyRows/0/0/contents/value", "\"d\""),
          ("/documentation/value/bodyRows/0/1/colspan", "1"),
          ("/documentation/value/bodyRows/0/1/rowspan", "2"),
          ("/documentation/value/bodyRows/0/1/contents/type", "\"String\""),
          ("/documentation/value/bodyRows/0/1/contents/value", "\"e\\n\\n\""),
          ("/documentation/value/bodyRows/1/0/colspan", "1"),
          ("/documentation/value/bodyRows/1/0/rowspan", "1"),
          ("/documentation/value/bodyRows/1/0/contents/type", "\"String\""),
          ("/documentation/value/bodyRows/1/0/contents/value", "\"f\""),
          ("/documentation/value/bodyRows/1/1/colspan", "1"),
          ("/documentation/value/bodyRows/1/1/rowspan", "1"),
          ("/documentation/value/bodyRows/1/1/contents/type", "\"String\""),
          ("/documentation/value/bodyRows/1/1/contents/value", "\"g\"")
        ]

  Spec.describe s "since" $ do
    Spec.it s "defaults to absent" $ do
      check s "" [("/since", "")]

    Spec.it s "works with a version" $ do
      check
        s
        """
        -- | Docs
        --
        -- @since 1.2.3
        module M where
        """
        [ ("/since/version", "[1,2,3]")
        ]

    Spec.it s "works with a package and version" $ do
      check
        s
        """
        -- | Docs
        --
        -- @since base-4.16.0
        module M where
        """
        [ ("/since/package", "\"base\""),
          ("/since/version", "[4,16,0]")
        ]

  Spec.describe s "name" $ do
    Spec.it s "defaults to absent" $ do
      check s "" [("/name", "")]

    Spec.it s "works with a simple module name" $ do
      check
        s
        "module M where"
        [ ("/name/location/line", "1"),
          ("/name/location/column", "8"),
          ("/name/value", "\"M\"")
        ]

    Spec.it s "works with a complex module name" $ do
      check
        s
        "module Foo.Bar where"
        [ ("/name/location/line", "1"),
          ("/name/location/column", "8"),
          ("/name/value", "\"Foo.Bar\"")
        ]

  Spec.describe s "warning" $ do
    Spec.it s "defaults to absent" $ do
      check s "" [("/warning", "")]

    Spec.it s "works with deprecated" $ do
      check
        s
        """
        module M {-# deprecated "foo" #-} where
        """
        [ ("/warning/category", "\"deprecations\""),
          ("/warning/value", "\"foo\"")
        ]

    Spec.it s "works with warning" $ do
      check
        s
        """
        module M {-# warning "foo" #-} where
        """
        [ ("/warning/category", "\"deprecations\""),
          ("/warning/value", "\"foo\"")
        ]

    Spec.it s "works with custom warning" $ do
      check
        s
        """
        module M {-# warning in "foo" "bar" #-} where
        """
        [ ("/warning/category", "\"foo\""),
          ("/warning/value", "\"bar\"")
        ]

    Spec.it s "works with empty list of messages" $ do
      check
        s
        """
        module M {-# warning [] #-} where
        """
        [ ("/warning/category", "\"deprecations\""),
          ("/warning/value", "\"\"")
        ]

    Spec.it s "works with list of messages" $ do
      check
        s
        """
        module M {-# warning ["foo", "bar"] #-} where
        """
        [ ("/warning/category", "\"deprecations\""),
          ("/warning/value", "\"foo\\nbar\"")
        ]

  Spec.describe s "export ordering" $ do
    Spec.it s "orders items by export list" $ do
      check
        s
        """
        module M ( y, x ) where
        x = ()
        y = ()
        """
        [ ("/items/0/value/name", "\"y\""),
          ("/items/1/value/name", "\"x\"")
        ]

    Spec.it s "creates section heading items from export groups" $ do
      check
        s
        """
        module M
          ( -- * Section One
            x
          ) where
        x = ()
        """
        [ ("/items/0/value/kind/type", "\"DocumentationChunk\""),
          ("/items/0/value/documentation/type", "\"Header\""),
          ("/items/1/value/name", "\"x\"")
        ]

    Spec.it s "creates inline doc items from export docs" $ do
      check
        s
        """
        module M
          ( -- | Some docs
            x
          ) where
        x = ()
        """
        [ ("/items/0/value/kind/type", "\"DocumentationChunk\""),
          ("/items/0/value/documentation/type", "\"Paragraph\""),
          ("/items/1/value/name", "\"x\"")
        ]

    Spec.it s "creates unresolved export items for module re-exports" $ do
      check
        s
        """
        module M ( module M, x ) where
        x = ()
        """
        [ ("/items/0/value/kind/type", "\"UnresolvedExport\""),
          ("/items/0/value/name", "\"M\""),
          ("/items/0/value/signature", "\"module\""),
          ("/items/1/value/name", "\"x\"")
        ]

    Spec.it s "creates unresolved export items for missing declarations" $ do
      check
        s
        """
        module M ( missing ) where
        """
        [ ("/items/0/value/kind/type", "\"UnresolvedExport\""),
          ("/items/0/value/name", "\"missing\"")
        ]

    Spec.it s "resolves named doc chunks into items" $ do
      check
        s
        """
        module M
          ( -- $foo
            unit,
          ) where

        -- $foo
        -- bar

        unit :: ()
        unit = ()
        """
        [ ("/items/0/value/kind/type", "\"DocumentationChunk\""),
          ("/items/0/value/documentation/value/value", "\"bar\""),
          ("/items/1/value/name", "\"unit\""),
          ("/items/1/value/kind/type", "\"Function\"")
        ]

    Spec.it s "renders unresolved named doc chunks as named card items" $ do
      check
        s
        """
        module M
          ( -- $x
            y
          ) where
        y = ()
        """
        [ ("/items/0/value/kind/type", "\"DocumentationChunk\""),
          ("/items/0/value/name", "\"$x\""),
          ("/items/0/value/documentation/type", "\"Empty\""),
          ("/items/1/value/name", "\"y\"")
        ]

    Spec.it s "creates metadata items for export-level doc comments" $ do
      check
        s
        """
        module M
          ( x -- ^ export doc
          ) where
        x = ()
        """
        [ ("/items/0/value/name", "\"x\""),
          ("/items/1/value/kind/type", "\"DocumentationChunk\""),
          ("/items/1/value/documentation/type", "\"Paragraph\""),
          ("/items/1/value/documentation/value/value", "\"export doc\"")
        ]

    Spec.it s "creates metadata items for export-level warnings" $ do
      check
        s
        """
        module M ( {-# warning "deprecated" #-} x ) where
        x = ()
        """
        [ ("/items/0/value/name", "\"x\""),
          ("/items/1/value/kind/type", "\"DocumentationChunk\""),
          ("/items/1/value/documentation/type", "\"Paragraph\"")
        ]

    Spec.it s "deduplicates repeated exports" $ do
      check
        s
        """
        module M ( x, x ) where
        x = ()
        """
        [ ("/items/0/value/name", "\"x\""),
          ("/items/0/value/kind/type", "\"Function\"")
        ]

    Spec.it s "places implicit items after exported items" $ do
      check
        s
        """
        module M ( MyClass ) where
        class MyClass a
        instance MyClass Int
        """
        [ ("/items/0/value/name", "\"MyClass a\""),
          ("/items/0/value/visibility/type", "\"Exported\""),
          ("/items/1/value/visibility/type", "\"Implicit\"")
        ]

  Spec.describe s "named chunks" $ do
    Spec.it s "creates items for unreferenced named chunks" $ do
      check
        s
        """
        -- $a
        -- b
        x=0
        """
        [ ("/items/0/value/name", "\"$a\""),
          ("/items/0/value/documentation/type", "\"Paragraph\""),
          ("/items/0/value/documentation/value/value", "\"b\""),
          ("/items/1/value/name", "\"x\"")
        ]

    Spec.it s "preceding doc comment does not leak past named chunk" $ do
      check
        s
        """
        module M
          ( -- $bar
            unit,
          ) where

        -- | foo
        -- $bar
        -- qux
        unit :: ()
        unit = ()
        """
        [ ("/items/0/value/kind/type", "\"DocumentationChunk\""),
          ("/items/0/value/documentation/value/value", "\"qux\""),
          ("/items/1/value/name", "\"unit\""),
          ("/items/1/value/documentation/type", "\"Empty\"")
        ]

    Spec.it s "preceding doc comment does not leak past unreferenced named chunk" $ do
      check
        s
        """
        -- | foo
        -- $a
        -- chunk
        x=0
        """
        [ ("/items/0/value/name", "\"$a\""),
          ("/items/0/value/documentation/value/value", "\"chunk\""),
          ("/items/1/value/name", "\"x\""),
          ("/items/1/value/documentation/type", "\"Empty\"")
        ]

    Spec.it s "creates items for unreferenced named chunks with explicit exports" $ do
      check
        s
        """
        module M
          ( -- $foo
            unit,
          ) where

        -- $foo
        -- bar

        -- $baz
        -- quux

        unit :: ()
        unit = ()
        """
        [ ("/items/0/value/kind/type", "\"DocumentationChunk\""),
          ("/items/0/value/documentation/value/value", "\"bar\""),
          ("/items/1/value/name", "\"unit\""),
          ("/items/2/value/name", "\"$baz\""),
          ("/items/2/value/documentation/value/value", "\"quux\"")
        ]

  Spec.describe s "doc groups" $ do
    Spec.it s "converts doc groups in the module body to headings" $ do
      check
        s
        """
        a = 1
        -- * My Section
        c = 2
        """
        [ ("/items/0/value/name", "\"a\""),
          ("/items/1/value/kind/type", "\"DocumentationChunk\""),
          ("/items/1/value/documentation/type", "\"Header\""),
          ("/items/1/value/documentation/value/level", "1"),
          ("/items/1/value/documentation/value/title/type", "\"Paragraph\""),
          ("/items/1/value/documentation/value/title/value/value", "\"My Section\""),
          ("/items/2/value/name", "\"c\"")
        ]

    Spec.it s "handles level 2 doc groups" $ do
      check
        s
        """
        -- ** Sub Section
        x = ()
        """
        [ ("/items/0/value/kind/type", "\"DocumentationChunk\""),
          ("/items/0/value/documentation/type", "\"Header\""),
          ("/items/0/value/documentation/value/level", "2"),
          ("/items/0/value/documentation/value/title/value/value", "\"Sub Section\""),
          ("/items/1/value/name", "\"x\"")
        ]

  Spec.describe s "imports" $ do
    Spec.it s "defaults to empty list" $ do
      check s "" [("/imports", "[]")]

    Spec.it s "works with a simple import" $ do
      check
        s
        "import Data.List"
        [ ("/imports/0/name", "\"Data.List\"")
        ]

    Spec.it s "works with multiple imports" $ do
      check
        s
        """
        import Data.List
        import Data.Map
        """
        [ ("/imports/0/name", "\"Data.List\""),
          ("/imports/1/name", "\"Data.Map\"")
        ]

    Spec.it s "works with an alias" $ do
      check
        s
        "import Data.List as List"
        [ ("/imports/0/name", "\"Data.List\""),
          ("/imports/0/alias", "\"List\"")
        ]

    Spec.it s "works with a package import" $ do
      check
        s
        "{-# language PackageImports #-} import \"base\" Data.List"
        [ ("/imports/0/name", "\"Data.List\""),
          ("/imports/0/package", "\"base\"")
        ]

    Spec.it s "works with a qualified import" $ do
      check
        s
        "import qualified Data.Map"
        [ ("/imports/0/name", "\"Data.Map\"")
        ]

    Spec.it s "works with a qualified import with alias" $ do
      check
        s
        "import qualified Data.Map as Map"
        [ ("/imports/0/name", "\"Data.Map\""),
          ("/imports/0/alias", "\"Map\"")
        ]

  Spec.describe s "items" $ do
    Spec.it s "defaults to empty list" $ do
      check s "" [("/items", "[]")]

    Spec.describe s "function" $ do
      -- Note that we call this a "function" because GHC does. Obviously it's
      -- just a value.

      Spec.it s "works with one" $ do
        check
          s
          "x = 0"
          [ ("/items/0/location/line", "1"),
            ("/items/0/location/column", "1"),
            ("/items/0/value/key", "0"),
            ("/items/0/value/kind/type", "\"Function\""),
            ("/items/0/value/name", "\"x\""),
            ("/items/0/value/documentation/type", "\"Empty\"")
          ]

      Spec.it s "works with two" $ do
        check
          s
          """
          x = 0
          y = 1
          """
          [ ("/items/0/location/line", "1"),
            ("/items/0/location/column", "1"),
            ("/items/0/value/key", "0"),
            ("/items/0/value/name", "\"x\""),
            ("/items/1/location/line", "2"),
            ("/items/1/location/column", "1"),
            ("/items/1/value/key", "1"),
            ("/items/1/value/name", "\"y\"")
          ]

      Spec.it s "works with documentation before" $ do
        check
          s
          """
          -- | x
          y = 0
          """
          [ ("/items/0/value/name", "\"y\""),
            ("/items/0/value/documentation/type", "\"Paragraph\""),
            ("/items/0/value/documentation/value/type", "\"String\""),
            ("/items/0/value/documentation/value/value", "\"x\"")
          ]

      Spec.it s "works with documentation after" $ do
        check
          s
          """
          x = 0
          -- ^ y
          """
          [ ("/items/0/value/name", "\"x\""),
            ("/items/0/value/documentation/type", "\"Paragraph\""),
            ("/items/0/value/documentation/value/type", "\"String\""),
            ("/items/0/value/documentation/value/value", "\"y\"")
          ]

      Spec.it s "works with signature" $ do
        check
          s
          """
          x :: Int
          x = 0
          """
          [ ("/items/0/value/name", "\"x\""),
            ("/items/0/value/signature", "\"Int\"")
          ]

      Spec.it s "works with @since annotation" $ do
        check
          s
          """
          -- | Docs
          --
          -- @since 1.2.3
          x :: Int
          x = 0
          """
          [ ("/items/0/value/name", "\"x\""),
            ("/items/0/value/since/version", "[1,2,3]")
          ]

      Spec.it s "works with @since annotation with package" $ do
        check
          s
          """
          -- | Docs
          --
          -- @since base-4.16.0
          x :: Int
          x = 0
          """
          [ ("/items/0/value/name", "\"x\""),
            ("/items/0/value/since/package", "\"base\""),
            ("/items/0/value/since/version", "[4,16,0]")
          ]

      Spec.it s "defaults to no @since" $ do
        check
          s
          "x = 0"
          [ ("/items/0/value/since", "")
          ]

    Spec.describe s "operator" $ do
      Spec.it s "works with a type signature" $ do
        check
          s
          "(+++) :: Int -> Int -> Int"
          [ ("/items/0/value/kind/type", "\"Operator\""),
            ("/items/0/value/name", "\"+++\""),
            ("/items/0/value/signature", "\"Int -> Int -> Int\"")
          ]

      Spec.it s "works with a binding" $ do
        check
          s
          "(+++) a b = a"
          [ ("/items/0/value/kind/type", "\"Operator\""),
            ("/items/0/value/name", "\"+++\"")
          ]

      Spec.it s "works with both signature and binding" $ do
        check
          s
          """
          (+++) :: Int -> Int -> Int
          (+++) a b = a
          """
          [ ("/items/0/value/kind/type", "\"Operator\""),
            ("/items/0/value/name", "\"+++\""),
            ("/items/0/value/signature", "\"Int -> Int -> Int\"")
          ]

      Spec.it s "does not affect regular functions" $ do
        check
          s
          "f :: Int -> Int"
          [ ("/items/0/value/kind/type", "\"Function\""),
            ("/items/0/value/name", "\"f\"")
          ]

    Spec.it s "open type family" $ do
      check s "{-# language TypeFamilies #-} type family A" [("/items/0/value/kind/type", "\"OpenTypeFamily\"")]

    Spec.it s "closed type family" $ do
      check s "{-# language TypeFamilies #-} type family B where" [("/items/0/value/kind/type", "\"ClosedTypeFamily\"")]

    Spec.it s "closed type family with instance" $ do
      check
        s
        "{-# language TypeFamilies #-} type family C a where C () = ()"
        [ ("/items/0/value/kind/type", "\"ClosedTypeFamily\""),
          ("/items/1/value/kind/type", "\"TypeFamilyInstance\""),
          ("/items/1/value/parentKey", "0"),
          ("/items/1/value/signature", "\"C () = ()\"")
        ]

    Spec.it s "closed type family with multiple equations" $ do
      check
        s
        "{-# language TypeFamilies, DataKinds #-} type family IsUnit a where IsUnit () = 'True; IsUnit _ = 'False"
        [ ("/items/1/value/signature", "\"IsUnit () = 'True\""),
          ("/items/2/value/signature", "\"IsUnit _ = 'False\"")
        ]

    Spec.it s "data family" $ do
      check s "{-# language TypeFamilies #-} data family F" [("/items/0/value/kind/type", "\"DataFamily\"")]

    Spec.it s "type synonym" $ do
      check
        s
        "type G = ()"
        [ ("/items/0/value/kind/type", "\"TypeSynonym\""),
          ("/items/0/value/signature", "\"= ()\"")
        ]

    Spec.it s "type synonym with type variable" $ do
      check
        s
        "type G a = [a]"
        [ ("/items/0/value/kind/type", "\"TypeSynonym\""),
          ("/items/0/value/signature", "\"a = [a]\"")
        ]

    Spec.it s "data" $ do
      check s "data H" [("/items/0/value/kind/type", "\"DataType\"")]

    Spec.it s "data with type variable" $ do
      check
        s
        "data T a"
        [ ("/items/0/value/kind/type", "\"DataType\""),
          ("/items/0/value/name", "\"T\""),
          ("/items/0/value/signature", "\"a\"")
        ]

    Spec.it s "data with multiple type variables" $ do
      check
        s
        "data E a b"
        [ ("/items/0/value/kind/type", "\"DataType\""),
          ("/items/0/value/name", "\"E\""),
          ("/items/0/value/signature", "\"a b\"")
        ]

    Spec.it s "newtype with type variable" $ do
      check
        s
        "newtype N a = MkN a"
        [ ("/items/0/value/kind/type", "\"Newtype\""),
          ("/items/0/value/name", "\"N\""),
          ("/items/0/value/signature", "\"a\"")
        ]

    Spec.it s "GADT with type variable" $ do
      check
        s
        "data G a where MkG :: a -> G a"
        [ ("/items/0/value/kind/type", "\"DataType\""),
          ("/items/0/value/name", "\"G\""),
          ("/items/0/value/signature", "\"a\"")
        ]

    Spec.it s "data constructor" $ do
      check
        s
        "data I = J"
        [ ("/items/0/value/kind/type", "\"DataType\""),
          ("/items/1/value/kind/type", "\"DataConstructor\"")
        ]

    Spec.it s "infix data constructor" $ do
      check
        s
        "data T = Int :+: Bool"
        [ ("/items/0/value/kind/type", "\"DataType\""),
          ("/items/1/value/kind/type", "\"DataConstructor\""),
          ("/items/1/value/name", "\":+:\""),
          ("/items/1/value/signature", "\"Int -> Bool -> T\"")
        ]

    Spec.it s "data constructor with doc" $ do
      check
        s
        "data I2 = {- | x -} J2"
        [ ("/items/1/value/kind/type", "\"DataConstructor\""),
          ("/items/1/value/name", "\"J2\""),
          ("/items/1/value/documentation/type", "\"Paragraph\""),
          ("/items/1/value/documentation/value/type", "\"String\""),
          ("/items/1/value/documentation/value/value", "\"x \""),
          ("/items/1/value/signature", "\"I2\"")
        ]

    Spec.it s "data constructor GADT" $ do
      check
        s
        "data K where L :: K"
        [ ("/items/0/value/kind/type", "\"DataType\""),
          ("/items/1/value/kind/type", "\"GADTConstructor\"")
        ]

    Spec.it s "GADT with multiple constructor names" $ do
      check
        s
        "data T where A, B :: Int -> T"
        [ ("/items/0/value/kind/type", "\"DataType\""),
          ("/items/0/value/name", "\"T\""),
          ("/items/1/value/kind/type", "\"GADTConstructor\""),
          ("/items/1/value/name", "\"A\""),
          ("/items/1/value/signature", "\"Int -> T\""),
          ("/items/2/value/kind/type", "\"Argument\""),
          ("/items/2/value/parentKey", "1"),
          ("/items/3/value/kind/type", "\"GADTConstructor\""),
          ("/items/3/value/name", "\"B\""),
          ("/items/3/value/signature", "\"Int -> T\""),
          ("/items/4/value/kind/type", "\"Argument\""),
          ("/items/4/value/parentKey", "3")
        ]

    Spec.it s "data constructor GADT with doc" $ do
      check
        s
        """
        data K2 where
          -- | d
          L2 :: K2
        """
        [ ("/items/1/value/kind/type", "\"GADTConstructor\""),
          ("/items/1/value/name", "\"L2\""),
          ("/items/1/value/documentation/type", "\"Paragraph\""),
          ("/items/1/value/documentation/value/type", "\"String\""),
          ("/items/1/value/documentation/value/value", "\"d\""),
          ("/items/1/value/signature", "\"K2\"")
        ]

    Spec.it s "data constructor with arg doc" $ do
      check
        s
        """
        data T2
          = C2
            Int -- ^ arg doc
            Bool
        """
        [ ("/items/1/value/name", "\"C2\""),
          ("/items/1/value/signature", "\"Int -> Bool -> T2\"")
        ]

    Spec.it s "data constructor GADT with arg doc" $ do
      check
        s
        """
        data T3 where
          C3 ::
            Int -- ^ arg doc
            -> T3
        """
        [ ("/items/1/value/name", "\"C3\""),
          ("/items/1/value/signature", "\"Int -> T3\"")
        ]

    Spec.it s "data constructor with record field doc" $ do
      check
        s
        """
        data T4 = C4
          { -- | field doc
            f4 :: Int
          }
        """
        [ ("/items/1/value/name", "\"C4\""),
          ("/items/1/value/signature", "\"{ f4 :: Int } -> T4\"")
        ]

    Spec.it s "data constructor with multiple record fields" $ do
      check
        s
        "data T = C { f1 :: Int, f2 :: Bool }"
        [ ("/items/1/value/signature", "\"{ f1 :: Int\\n, f2 :: Bool\\n} -> T\"")
        ]

    Spec.it s "data constructor with existential" $ do
      check
        s
        "{-# language ExistentialQuantification #-} data T5 = forall a. C5 a"
        [ ("/items/1/value/name", "\"C5\""),
          ("/items/1/value/signature", "\"forall a. a -> T5\"")
        ]

    Spec.it s "data constructor with context" $ do
      check
        s
        "{-# language ExistentialQuantification, FlexibleContexts #-} data T6 = forall a. Show a => C6 a"
        [ ("/items/1/value/name", "\"C6\""),
          ("/items/1/value/signature", "\"forall a. Show a => a -> T6\"")
        ]

    Spec.it s "type data" $ do
      check s "{-# language TypeData #-} type data L" [("/items/0/value/kind/type", "\"TypeData\"")]

    Spec.it s "type data constructor" $ do
      check
        s
        "{-# language TypeData #-} type data M = N"
        [ ("/items/0/value/kind/type", "\"TypeData\""),
          ("/items/1/value/kind/type", "\"DataConstructor\"")
        ]

    Spec.it s "type data constructor GADT" $ do
      check
        s
        "{-# language TypeData #-} type data O where P :: O"
        [ ("/items/0/value/kind/type", "\"TypeData\""),
          ("/items/1/value/kind/type", "\"GADTConstructor\"")
        ]

    Spec.it s "newtype" $ do
      check
        s
        "newtype Q = R ()"
        [ ("/items/0/value/kind/type", "\"Newtype\""),
          ("/items/1/value/kind/type", "\"DataConstructor\"")
        ]

    Spec.it s "record field" $ do
      check
        s
        "data S = T { u :: () }"
        [ ("/items/0/value/kind/type", "\"DataType\""),
          ("/items/1/value/kind/type", "\"DataConstructor\""),
          ("/items/2/value/kind/type", "\"RecordField\""),
          ("/items/2/value/signature", "\"()\"")
        ]

    Spec.it s "record field with strict annotation" $ do
      check
        s
        "data A = B { c :: !Int }"
        [ ("/items/0/value/kind/type", "\"DataType\""),
          ("/items/1/value/kind/type", "\"DataConstructor\""),
          ("/items/2/value/kind/type", "\"RecordField\""),
          ("/items/2/value/name", "\"c\""),
          ("/items/2/value/signature", "\"!Int\"")
        ]

    Spec.it s "record field with lazy annotation" $ do
      check
        s
        "data A = B { c :: ~Int }"
        [ ("/items/2/value/kind/type", "\"RecordField\""),
          ("/items/2/value/signature", "\"~Int\"")
        ]

    Spec.it s "record field with UNPACK pragma" $ do
      check
        s
        "data A = B { c :: {-# UNPACK #-} !Int }"
        [ ("/items/2/value/kind/type", "\"RecordField\""),
          ("/items/2/value/signature", "\"{-# UNPACK #-} !Int\"")
        ]

    Spec.it s "record field GADT" $ do
      check
        s
        "data V where W :: { x :: () } -> V"
        [ ("/items/0/value/kind/type", "\"DataType\""),
          ("/items/1/value/kind/type", "\"GADTConstructor\""),
          ("/items/2/value/kind/type", "\"RecordField\"")
        ]

    Spec.it s "class" $ do
      check s "class Y" [("/items/0/value/kind/type", "\"Class\"")]

    Spec.it s "class with type variable" $ do
      check
        s
        "class C a"
        [ ("/items/0/value/kind/type", "\"Class\""),
          ("/items/0/value/name", "\"C a\"")
        ]

    Spec.it s "class instance" $ do
      check
        s
        """
        class Z a
        instance Z ()
        """
        [ ("/items/1/value/kind/type", "\"ClassInstance\""),
          ("/items/1/value/parentKey", "0")
        ]

    Spec.it s "class instance parent is local type" $ do
      check
        s
        """
        class C a
        data T
        instance C T
        """
        [ ("/items/2/value/kind/type", "\"ClassInstance\""),
          ("/items/2/value/name", "\"C T\""),
          ("/items/2/value/parentKey", "1")
        ]

    Spec.it s "class instance parent is local class when type is external" $ do
      check
        s
        """
        class C a
        instance C Int
        """
        [ ("/items/1/value/kind/type", "\"ClassInstance\""),
          ("/items/1/value/parentKey", "0")
        ]

    Spec.it s "standalone deriving parent is local class when type is external" $ do
      check
        s
        """
        {-# language StandaloneDeriving #-}
        class C a where
        deriving instance C Int
        """
        [ ("/items/1/value/kind/type", "\"StandaloneDeriving\""),
          ("/items/1/value/parentKey", "0")
        ]

    Spec.it s "data instance" $ do
      check
        s
        """
        {-# language TypeFamilies #-}
        data family A a
        data instance A ()
        """
        [ ("/items/1/value/kind/type", "\"DataFamilyInstance\""),
          ("/items/1/value/parentKey", "0")
        ]

    Spec.it s "data instance constructor" $ do
      check
        s
        """
        {-# language TypeFamilies #-}
        data family B a
        data instance B () = C
        """
        [ ("/items/1/value/kind/type", "\"DataFamilyInstance\""),
          ("/items/1/value/parentKey", "0"),
          ("/items/2/value/kind/type", "\"DataConstructor\""),
          ("/items/2/value/parentKey", "1"),
          ("/items/2/value/signature", "\"B ()\"")
        ]

    Spec.it s "data instance constructor GADT" $ do
      check
        s
        """
        {-# language TypeFamilies #-}
        data family D
        data instance D where E :: D
        """
        [ ("/items/1/value/kind/type", "\"DataFamilyInstance\""),
          ("/items/1/value/parentKey", "0"),
          ("/items/2/value/kind/type", "\"GADTConstructor\""),
          ("/items/2/value/parentKey", "1")
        ]

    Spec.it s "newtype instance" $ do
      check
        s
        """
        {-# language TypeFamilies #-}
        data family F
        newtype instance F = G ()
        """
        [ ("/items/1/value/kind/type", "\"DataFamilyInstance\""),
          ("/items/1/value/parentKey", "0"),
          ("/items/2/value/kind/type", "\"DataConstructor\""),
          ("/items/2/value/parentKey", "1"),
          ("/items/2/value/signature", "\"() -> F\"")
        ]

    Spec.it s "newtype instance with type argument" $ do
      check
        s
        """
        {-# language TypeFamilies #-}
        data family Collection a
        newtype instance Collection Int = CollectionInt [Int]
        """
        [ ("/items/2/value/kind/type", "\"DataConstructor\""),
          ("/items/2/value/signature", "\"[Int] -> Collection Int\"")
        ]

    Spec.it s "newtype instance GADT" $ do
      check
        s
        """
        {-# language TypeFamilies #-}
        data family H
        newtype instance H where I :: () -> H
        """
        [ ("/items/1/value/kind/type", "\"DataFamilyInstance\""),
          ("/items/1/value/parentKey", "0"),
          ("/items/2/value/kind/type", "\"GADTConstructor\""),
          ("/items/2/value/parentKey", "1")
        ]

    Spec.it s "type instance" $ do
      check
        s
        """
        {-# language TypeFamilies #-}
        type family J
        type instance J = ()
        """
        [ ("/items/1/value/kind/type", "\"TypeFamilyInstance\""),
          ("/items/1/value/parentKey", "0")
        ]

    Spec.it s "standalone deriving" $ do
      check
        s
        """
        data L
        deriving instance Show L
        """
        [ ("/items/1/value/kind/type", "\"StandaloneDeriving\""),
          ("/items/1/value/name", "\"Show L\""),
          ("/items/1/value/parentKey", "0")
        ]

    Spec.it s "standalone deriving stock" $ do
      check
        s
        """
        {-# language DerivingStrategies #-}
        data M
        deriving stock instance Show M
        """
        [ ("/items/1/value/kind/type", "\"StandaloneDeriving\""),
          ("/items/1/value/name", "\"Show M\""),
          ("/items/1/value/parentKey", "0")
        ]

    Spec.it s "standalone deriving newtype" $ do
      check
        s
        """
        {-# language DerivingStrategies #-}
        newtype N = MkN Int
        deriving newtype instance Show N
        """
        [ ("/items/2/value/kind/type", "\"Argument\""),
          ("/items/2/value/parentKey", "1"),
          ("/items/3/value/kind/type", "\"StandaloneDeriving\""),
          ("/items/3/value/name", "\"Show N\""),
          ("/items/3/value/parentKey", "0")
        ]

    Spec.it s "standalone deriving anyclass" $ do
      check
        s
        """
        {-# language DerivingStrategies, DeriveAnyClass #-}
        class O a
        data W2
        deriving anyclass instance O W2
        """
        [ ("/items/2/value/kind/type", "\"StandaloneDeriving\""),
          ("/items/2/value/name", "\"O W2\""),
          ("/items/2/value/parentKey", "1"),
          ("/items/2/value/signature", "\"anyclass\"")
        ]

    Spec.it s "standalone deriving via" $ do
      check
        s
        """
        {-# language DerivingStrategies, DerivingVia #-}
        newtype P = MkP Int
        deriving via Int instance Show P
        """
        [ ("/items/2/value/kind/type", "\"Argument\""),
          ("/items/2/value/parentKey", "1"),
          ("/items/3/value/kind/type", "\"StandaloneDeriving\""),
          ("/items/3/value/name", "\"Show P\""),
          ("/items/3/value/parentKey", "0")
        ]

    Spec.it s "data deriving" $ do
      check
        s
        "data R deriving Show"
        [ ("/items/0/value/kind/type", "\"DataType\""),
          ("/items/1/value/kind/type", "\"DerivedInstance\""),
          ("/items/1/value/name", "\"Show\"")
        ]

    Spec.it s "data deriving multiple" $ do
      check
        s
        "data R2 deriving (Show, Eq)"
        [ ("/items/1/value/kind/type", "\"DerivedInstance\""),
          ("/items/1/value/name", "\"Show\""),
          ("/items/2/value/kind/type", "\"DerivedInstance\""),
          ("/items/2/value/name", "\"Eq\"")
        ]

    Spec.it s "data deriving stock" $ do
      check
        s
        """
        {-# LANGUAGE DerivingStrategies #-}
        data R3 deriving stock Show
        """
        [ ("/items/1/value/kind/type", "\"DerivedInstance\""),
          ("/items/1/value/name", "\"Show\""),
          ("/items/1/value/signature", "\"stock\"")
        ]

    Spec.it s "data deriving via" $ do
      check
        s
        """
        {-# LANGUAGE DerivingStrategies, DerivingVia #-}
        data R5 deriving Show via ()
        """
        [ ("/items/1/value/kind/type", "\"DerivedInstance\""),
          ("/items/1/value/name", "\"Show\""),
          ("/items/1/value/signature", "\"via ()\"")
        ]

    Spec.it s "data deriving no strategy" $ do
      check
        s
        "data R6 deriving Show"
        [ ("/items/1/value/kind/type", "\"DerivedInstance\""),
          ("/items/1/value/name", "\"Show\""),
          ("/items/1/value/signature", "\"derived\"")
        ]

    Spec.it s "data GADT deriving" $ do
      check s "data T where deriving Show" []

    Spec.it s "newtype deriving" $ do
      check s "newtype Z = A () deriving Show" []

    Spec.it s "newtype GADT deriving" $ do
      check s "newtype C where D :: () -> C deriving Show" []

    Spec.it s "function" $ do
      check s "e f = ()" []

    Spec.it s "infix function" $ do
      check s "g `h` i = ()" []

    Spec.it s "infix operator" $ do
      check s "j % k = ()" []

    Spec.it s "prefix operator" $ do
      check s "(&) = ()" []

    Spec.it s "strict variable" $ do
      check s "f !l = ()" []

    Spec.it s "lazy pattern" $ do
      check s "~m = ()" []

    Spec.it s "as pattern" $ do
      check s "n@o = ()" []

    Spec.it s "parenthesized pattern" $ do
      check s "(p) = ()" []

    Spec.it s "bang pattern" $ do
      check s "f (!q) = ()" []

    Spec.it s "list pattern" $ do
      check s "[r] = [()]" []

    Spec.it s "tuple pattern" $ do
      check s "(s, t) = ((), ())" []

    Spec.it s "anonymous sum pattern" $ do
      check
        s
        """
        {-# language UnboxedSums #-}
        (# u | #) = (# () | #)
        """
        []

    Spec.it s "prefix constructor pattern" $ do
      check s "Just v = Just ()" []

    Spec.it s "record constructor pattern" $ do
      check
        s
        """
        data W = W { wx :: () }
        W { wx = y } = W ()
        """
        []

    Spec.it s "punned record pattern" $ do
      check
        s
        """
        {-# language NoFieldSelectors #-}
        data Z = Z { a :: () }
        Z { a } = Z ()
        """
        []

    Spec.it s "wild card record pattern" $ do
      check
        s
        """
        {-# language RecordWildCards, NoFieldSelectors #-}
        data B = B { bf :: () }
        B { .. } = B ()
        """
        []

    Spec.it s "infix constructor pattern" $ do
      check s "(c : d) = [()]" []

    Spec.it s "view pattern" $ do
      check s "{-# language ViewPatterns #-} (id -> f) = ()" []

    Spec.it s "splice pattern" $ do
      check
        s
        """
        {-# language TemplateHaskell #-}
        import Language.Haskell.TH
        $( varP (mkName "y") ) = ()
        """
        []

    Spec.it s "literal pattern" $ do
      check s "'h' = 'i'" []

    Spec.it s "natural pattern" $ do
      check s "0 = (0 :: Int)" []

    Spec.it s "n+k pattern" $ do
      check
        s
        """
        {-# language NPlusKPatterns #-}
        (i + 1) = (0 :: Int)
        """
        []

    Spec.it s "signature pattern" $ do
      check s "(j :: ()) = ()" []

    Spec.it s "bidirectional pattern synonym" $ do
      check
        s
        "{-# language PatternSynonyms #-} pattern L = ()"
        [("/items/0/value/kind/type", "\"PatternSynonym\"")]

    Spec.it s "unidirectional pattern synonym" $ do
      check
        s
        "{-# language PatternSynonyms #-} pattern M <- ()"
        [("/items/0/value/kind/type", "\"PatternSynonym\"")]

    Spec.it s "explicitly bidirectional pattern synonym" $ do
      check
        s
        "{-# language PatternSynonyms #-} pattern N <- () where N = ()"
        [("/items/0/value/kind/type", "\"PatternSynonym\"")]

    Spec.it s "type signature" $ do
      check
        s
        """
        o :: ()
        o = ()
        """
        []

    Spec.it s "pattern type signature" $ do
      check
        s
        """
        {-# language PatternSynonyms #-}
        pattern P :: ()
        pattern P = ()
        """
        [("/items/0/value/kind/type", "\"PatternSynonym\"")]

    Spec.it s "pattern synonym signature without binding" $ do
      check
        s
        """
        {-# language PatternSynonyms #-}
        pattern Q :: a
        """
        [("/items/0/value/kind/type", "\"PatternSynonym\"")]

    Spec.describe s "method signature" $ do
      Spec.it s "works" $ do
        check
          s
          """
          class C a where
            m :: a
          """
          [ ("/items/0/value/key", "0"),
            ("/items/0/value/kind/type", "\"Class\""),
            ("/items/1/value/key", "1"),
            ("/items/1/value/kind/type", "\"ClassMethod\""),
            ("/items/1/value/parentKey", "0"),
            ("/items/1/value/signature", "\"a\"")
          ]

      Spec.it s "works with documentation" $ do
        check
          s
          """
          class C a where
            -- | d
            m :: a
          """
          [ ("/items/1/value/documentation/type", "\"Paragraph\""),
            ("/items/1/value/documentation/value/type", "\"String\""),
            ("/items/1/value/documentation/value/value", "\"d\"")
          ]

    Spec.it s "class method with default" $ do
      check
        s
        """
        class C a where
          m :: a -> a
          m = id
        """
        [ ("/items/1/value/kind/type", "\"ClassMethod\""),
          ("/items/1/value/name", "\"m\""),
          ("/items/1/value/signature", "\"a -> a\"")
        ]

    Spec.it s "default method signature" $ do
      check
        s
        """
        {-# language DefaultSignatures #-}
        class S a where
          t :: a
          default t :: a
          t = undefined
        """
        [ ("/items/0/value/kind/type", "\"Class\""),
          ("/items/0/value/name", "\"S a\""),
          ("/items/0/value/key", "0"),
          ("/items/1/value/kind/type", "\"ClassMethod\""),
          ("/items/1/value/name", "\"t\""),
          ("/items/1/value/parentKey", "0"),
          ("/items/1/value/key", "1"),
          ("/items/2/value/kind/type", "\"DefaultMethodSignature\""),
          ("/items/2/value/name", "\"t\""),
          ("/items/2/value/parentKey", "1"),
          ("/items/2/value/signature", "\"a\"")
        ]

    Spec.it s "default method signature with doc" $ do
      check
        s
        """
        {-# language DefaultSignatures #-}
        class S a where
          t :: a
          -- | the default
          default t :: a
          t = undefined
        """
        [ ("/items/2/value/kind/type", "\"DefaultMethodSignature\""),
          ("/items/2/value/name", "\"t\""),
          ("/items/2/value/documentation/type", "\"Paragraph\""),
          ("/items/2/value/documentation/value/type", "\"String\""),
          ("/items/2/value/documentation/value/value", "\"the default\"")
        ]

    Spec.it s "fixity has parent set" $ do
      check
        s
        """
        (%) = id
        infixl 0 %
        """
        [ ("/items/0/value/name", "\"%\""),
          ("/items/0/value/kind/type", "\"Operator\""),
          ("/items/1/value/name", "\"%\""),
          ("/items/1/value/kind/type", "\"FixitySignature\""),
          ("/items/1/value/parentKey", "0"),
          ("/items/1/value/documentation/value/value", "\"infixl 0\"")
        ]

    Spec.it s "fixity preserves type signature" $ do
      check
        s
        """
        infixl 5 %
        (%) :: () -> () -> ()
        (%) _ _ = ()
        """
        [ ("/items/0/value/name", "\"%\""),
          ("/items/0/value/kind/type", "\"FixitySignature\""),
          ("/items/0/value/parentKey", "1"),
          ("/items/0/value/documentation/value/value", "\"infixl 5\""),
          ("/items/1/value/name", "\"%\""),
          ("/items/1/value/kind/type", "\"Operator\""),
          ("/items/1/value/signature", "\"() -> () -> ()\"")
        ]

    Spec.it s "fixity infixr" $ do
      check
        s
        """
        (%) = id
        infixr 9 %
        """
        [ ("/items/1/value/kind/type", "\"FixitySignature\""),
          ("/items/1/value/documentation/value/value", "\"infixr 9\"")
        ]

    Spec.it s "fixity infix" $ do
      check
        s
        """
        (%) = id
        infix 4 %
        """
        [ ("/items/1/value/kind/type", "\"FixitySignature\""),
          ("/items/1/value/documentation/value/value", "\"infix 4\"")
        ]

    Spec.it s "orphaned fixity has no parent" $ do
      check
        s
        """
        infixl 0 %
        """
        [ ("/items/0/value/name", "\"%\""),
          ("/items/0/value/kind/type", "\"FixitySignature\""),
          ("/items/0/value/parentKey", ""),
          ("/items/0/value/documentation/value/value", "\"infixl 0\"")
        ]

    Spec.it s "fixity preserves user documentation" $ do
      check
        s
        """
        (%) = id
        -- | user doc
        infixl 5 %
        """
        [ ("/items/1/value/kind/type", "\"FixitySignature\""),
          ("/items/1/value/documentation/type", "\"Append\""),
          ("/items/1/value/documentation/value/0/type", "\"Paragraph\""),
          ("/items/1/value/documentation/value/0/value/type", "\"String\""),
          ("/items/1/value/documentation/value/0/value/value", "\"user doc\""),
          ("/items/1/value/documentation/value/1/type", "\"Paragraph\""),
          ("/items/1/value/documentation/value/1/value/type", "\"String\""),
          ("/items/1/value/documentation/value/1/value/value", "\"infixl 5\"")
        ]

    Spec.it s "fixity on data constructor has no parent" $ do
      check
        s
        """
        data T = Int :+: Int
        infixl 6 :+:
        """
        [ ("/items/0/value/kind/type", "\"DataType\""),
          ("/items/0/value/key", "0"),
          ("/items/1/value/kind/type", "\"DataConstructor\""),
          ("/items/1/value/name", "\":+:\""),
          ("/items/1/value/parentKey", "0"),
          ("/items/1/value/key", "1"),
          ("/items/2/value/kind/type", "\"Argument\""),
          ("/items/2/value/parentKey", "1"),
          ("/items/3/value/kind/type", "\"Argument\""),
          ("/items/3/value/parentKey", "1"),
          ("/items/4/value/kind/type", "\"FixitySignature\""),
          ("/items/4/value/name", "\":+:\"")
        ]

    Spec.it s "inline pragma has parent set" $ do
      check
        s
        """
        i = ()
        {-# inline i #-}
        """
        [ ("/items/0/value/name", "\"i\""),
          ("/items/0/value/kind/type", "\"Function\""),
          ("/items/1/value/name", "\"i\""),
          ("/items/1/value/kind/type", "\"InlineSignature\""),
          ("/items/1/value/signature", "\"INLINE\""),
          ("/items/1/value/parentKey", "0")
        ]

    Spec.it s "inline pragma with phase control" $ do
      check
        s
        """
        j = ()
        {-# inline [1] j #-}
        """
        [ ("/items/1/value/kind/type", "\"InlineSignature\""),
          ("/items/1/value/signature", "\"INLINE [1]\""),
          ("/items/1/value/parentKey", "0")
        ]

    Spec.it s "inline pragma with inverted phase control" $ do
      check
        s
        """
        k = ()
        {-# inline [~2] k #-}
        """
        [ ("/items/1/value/kind/type", "\"InlineSignature\""),
          ("/items/1/value/signature", "\"INLINE [~2]\""),
          ("/items/1/value/parentKey", "0")
        ]

    Spec.it s "inline pragma with conlike modifier" $ do
      check
        s
        """
        k2 = ()
        {-# inline conlike k2 #-}
        """
        [ ("/items/1/value/kind/type", "\"InlineSignature\""),
          ("/items/1/value/signature", "\"INLINE CONLIKE\""),
          ("/items/1/value/parentKey", "0")
        ]

    Spec.it s "noinline pragma has parent set" $ do
      check
        s
        """
        l = ()
        {-# noinline l #-}
        """
        [ ("/items/0/value/name", "\"l\""),
          ("/items/0/value/kind/type", "\"Function\""),
          ("/items/1/value/name", "\"l\""),
          ("/items/1/value/kind/type", "\"InlineSignature\""),
          ("/items/1/value/signature", "\"NOINLINE\""),
          ("/items/1/value/parentKey", "0")
        ]

    Spec.it s "inlinable pragma" $ do
      check
        s
        """
        m = ()
        {-# inlinable m #-}
        """
        [ ("/items/1/value/kind/type", "\"InlineSignature\""),
          ("/items/1/value/signature", "\"INLINABLE\""),
          ("/items/1/value/parentKey", "0")
        ]

    Spec.it s "opaque pragma" $ do
      check
        s
        """
        n = ()
        {-# opaque n #-}
        """
        [ ("/items/1/value/kind/type", "\"InlineSignature\""),
          ("/items/1/value/signature", "\"OPAQUE\""),
          ("/items/1/value/parentKey", "0")
        ]

    Spec.it s "orphaned inline pragma has no parent" $ do
      check
        s
        """
        {-# inline x #-}
        """
        [ ("/items/0/value/name", "\"x\""),
          ("/items/0/value/kind/type", "\"InlineSignature\""),
          ("/items/0/value/signature", "\"INLINE\""),
          ("/items/0/value/parentKey", "")
        ]

    Spec.it s "specialize pragma" $ do
      check
        s
        """
        j2 :: a -> a
        j2 = id
        {-# specialize j2 :: () -> () #-}
        """
        [ ("/items/0/value/kind/type", "\"Function\""),
          ("/items/0/value/name", "\"j2\""),
          ("/items/1/value/kind/type", "\"SpecialiseSignature\""),
          ("/items/1/value/name", "\"j2\""),
          ("/items/1/value/parentKey", "0"),
          ("/items/1/value/signature", "\"() -> ()\"")
        ]

    Spec.it s "orphaned specialize pragma" $ do
      check
        s
        """
        {-# specialize j3 :: () -> () #-}
        """
        [ ("/items/0/value/kind/type", "\"SpecialiseSignature\""),
          ("/items/0/value/name", "\"j3\""),
          ("/items/0/value/signature", "\"() -> ()\"")
        ]

    Spec.it s "specialize instance pragma" $ do
      check
        s
        """
        class K2 a where
          k2m :: a
        instance K2 () where
          k2m = ()
          {-# specialize instance K2 () #-}
        """
        []

    Spec.it s "minimal pragma" $ do
      check
        s
        """
        class L2 a where
          l2m :: a
          {-# minimal l2m #-}
        """
        [ ("/items/0/value/kind/type", "\"Class\""),
          ("/items/0/value/name", "\"L2 a\""),
          ("/items/1/value/kind/type", "\"ClassMethod\""),
          ("/items/1/value/parentKey", "0"),
          ("/items/2/value/kind/type", "\"MinimalPragma\""),
          ("/items/2/value/parentKey", "0"),
          ("/items/2/value/signature", "\"l2m\"")
        ]

    Spec.it s "set cost center pragma" $ do
      check
        s
        """
        m = ()
        {-# scc m #-}
        """
        []

    Spec.it s "complete pragma" $ do
      check
        s
        """
        {-# language PatternSynonyms #-}
        pattern N2 :: ()
        pattern N2 = ()
        {-# complete N2 #-}
        """
        [ ("/items/0/value/kind/type", "\"PatternSynonym\""),
          ("/items/0/value/parentKey", "2"),
          ("/items/1/value/kind/type", "\"CompletePragma\""),
          ("/items/1/value/signature", "\"N2\"")
        ]

    Spec.it s "complete pragma with multiple patterns" $ do
      check
        s
        """
        {-# language PatternSynonyms #-}
        pattern Nil :: [a]
        pattern Nil = []
        pattern Cons :: a -> [a] -> [a]
        pattern Cons x xs = x : xs
        {-# complete Nil, Cons #-}
        """
        [ ("/items/0/value/kind/type", "\"PatternSynonym\""),
          ("/items/0/value/name", "\"Nil\""),
          ("/items/0/value/parentKey", "4"),
          ("/items/1/value/kind/type", "\"PatternSynonym\""),
          ("/items/1/value/name", "\"Cons\""),
          ("/items/1/value/parentKey", "4"),
          ("/items/2/value/kind/type", "\"CompletePragma\""),
          ("/items/2/value/signature", "\"Nil, Cons\"")
        ]

    Spec.it s "standalone kind signature" $ do
      check
        s
        """
        type O :: *
        data O
        """
        [ ("/items/0/value/kind/type", "\"DataType\""),
          ("/items/0/value/name", "\"O\""),
          ("/items/0/value/signature", "\"*\"")
        ]

    Spec.it s "standalone kind signature with data" $ do
      check
        s
        """
        type X :: a -> a
        data X a = X
        """
        [ ("/items/0/value/kind/type", "\"DataType\""),
          ("/items/0/value/name", "\"X a\""),
          ("/items/0/value/signature", "\"a -> a\""),
          ("/items/1/value/kind/type", "\"DataConstructor\""),
          ("/items/1/value/name", "\"X\""),
          ("/items/1/value/parentKey", "1")
        ]

    Spec.it s "standalone kind signature with newtype" $ do
      check
        s
        """
        type Phantom :: * -> *
        newtype Phantom a = MkPhantom ()
        """
        [ ("/items/0/value/kind/type", "\"Newtype\""),
          ("/items/0/value/name", "\"Phantom a\""),
          ("/items/0/value/signature", "\"* -> *\""),
          ("/items/1/value/kind/type", "\"DataConstructor\""),
          ("/items/1/value/name", "\"MkPhantom\""),
          ("/items/1/value/parentKey", "1")
        ]

    Spec.it s "standalone kind signature with type synonym" $ do
      check
        s
        """
        type T :: * -> *
        type T a = Maybe a
        """
        [ ("/items/0/value/kind/type", "\"TypeSynonym\""),
          ("/items/0/value/name", "\"T\""),
          ("/items/0/value/signature", "\"* -> *\"")
        ]

    Spec.it s "standalone kind signature with class" $ do
      check
        s
        """
        {-# LANGUAGE ConstraintKinds #-}
        type C :: * -> Constraint
        class C a
        """
        [ ("/items/0/value/kind/type", "\"Class\""),
          ("/items/0/value/name", "\"C a\""),
          ("/items/0/value/signature", "\"* -> Constraint\"")
        ]

    Spec.it s "standalone kind signature with type family" $ do
      check
        s
        """
        {-# LANGUAGE TypeFamilies #-}
        type F :: * -> *
        type family F a
        """
        [ ("/items/0/value/kind/type", "\"OpenTypeFamily\""),
          ("/items/0/value/name", "\"F\""),
          ("/items/0/value/signature", "\"* -> *\"")
        ]

    Spec.it s "default declaration" $ do
      check s "default ()" [("/items", "[]")]

    Spec.it s "named default declaration" $ do
      check
        s
        "{-# language NamedDefaults #-} default Num (Int, Double)"
        [ ("/items/0/value/kind/type", "\"Default\""),
          ("/items/0/value/name", "\"Num\""),
          ("/items/0/value/signature", "\"Int, Double\"")
        ]

    Spec.it s "foreign import" $ do
      check
        s
        "{-# language ForeignFunctionInterface #-} foreign import ccall \"\" p :: ()"
        [ ("/items/0/value/kind/type", "\"ForeignImport\""),
          ("/items/0/value/name", "\"p\""),
          ("/items/0/value/signature", "\"()\"")
        ]

    Spec.it s "foreign export" $ do
      check
        s
        "{-# language ForeignFunctionInterface #-} foreign export ccall q :: IO ()"
        [ ("/items/0/value/kind/type", "\"ForeignExport\""),
          ("/items/0/value/name", "\"q\""),
          ("/items/0/value/signature", "\"IO ()\"")
        ]

    Spec.it s "warning pragma has parent set" $ do
      check
        s
        """
        x = ()
        {-# warning x "w" #-}
        """
        [ ("/items/0/value/name", "\"x\""),
          ("/items/0/value/kind/type", "\"Function\""),
          ("/items/1/value/name", "\"x\""),
          ("/items/1/value/kind/type", "\"Warning\""),
          ("/items/1/value/parentKey", "0"),
          ("/items/1/value/documentation/value/value", "\"w\""),
          ("/items/1/value/signature", "\"deprecations\"")
        ]

    Spec.it s "warning pragma with multiple names" $ do
      check
        s
        """
        x = ()
        y = ()
        {-# warning x, y "z" #-}
        """
        [ ("/items/0/value/name", "\"x\""),
          ("/items/0/value/kind/type", "\"Function\""),
          ("/items/1/value/name", "\"y\""),
          ("/items/1/value/kind/type", "\"Function\""),
          ("/items/2/value/name", "\"x\""),
          ("/items/2/value/kind/type", "\"Warning\""),
          ("/items/2/value/parentKey", "0"),
          ("/items/2/value/documentation/value/value", "\"z\""),
          ("/items/3/value/name", "\"y\""),
          ("/items/3/value/kind/type", "\"Warning\""),
          ("/items/3/value/parentKey", "1"),
          ("/items/3/value/documentation/value/value", "\"z\"")
        ]

    Spec.it s "orphaned warning pragma has no parent" $ do
      check
        s
        """
        {-# warning x "w" #-}
        """
        [ ("/items/0/value/name", "\"x\""),
          ("/items/0/value/kind/type", "\"Warning\""),
          ("/items/0/value/parentKey", ""),
          ("/items/0/value/documentation/value/value", "\"w\""),
          ("/items/0/value/signature", "\"deprecations\"")
        ]

    Spec.it s "warning pragma with custom category" $ do
      check
        s
        """
        x = ()
        {-# warning in "c" x "w" #-}
        """
        [ ("/items/0/value/name", "\"x\""),
          ("/items/0/value/kind/type", "\"Function\""),
          ("/items/1/value/name", "\"x\""),
          ("/items/1/value/kind/type", "\"Warning\""),
          ("/items/1/value/parentKey", "0"),
          ("/items/1/value/documentation/value/value", "\"w\""),
          ("/items/1/value/signature", "\"c\"")
        ]

    Spec.it s "value annotation" $ do
      check
        s
        """
        x2 = ()
        {-# ann x2 () #-}
        """
        []

    Spec.it s "type annotation" $ do
      check
        s
        """
        data X4
        {-# ann type X4 () #-}
        """
        []

    Spec.it s "module annotation" $ do
      check s "{-# ann module () #-}" []

    Spec.it s "rules pragma" $ do
      check
        s
        """
        x4 :: a -> a
        x4 = id
        {-# rules "q" x4 = id #-}
        """
        [ ("/items/0/value/kind/type", "\"Function\""),
          ("/items/0/value/name", "\"x4\""),
          ("/items/1/value/kind/type", "\"Rule\""),
          ("/items/1/value/name", "\"q\""),
          ("/items/1/value/signature", "\"x4 = id\"")
        ]

    Spec.it s "splice declaration" $ do
      check
        s
        """
        {-# language TemplateHaskell #-}
        $( return [] )
        """
        [ ("/items/0/value/kind/type", "\"Splice\""),
          ("/items/0/value/signature", "\"$(return [])\"")
        ]

    Spec.it s "typed splice declaration" $ do
      check
        s
        """
        {-# language TemplateHaskell #-}
        $$(pure [])
        """
        [ ("/items/0/value/kind/type", "\"Splice\""),
          ("/items/0/value/signature", "\"$$(pure [])\"")
        ]

    Spec.it s "quasi-quote declaration" $ do
      check
        s
        """
        {-# language QuasiQuotes #-}
        [x||]
        """
        [ ("/items/0/value/kind/type", "\"Splice\""),
          ("/items/0/value/signature", "\"[x||]\"")
        ]

    Spec.it s "role annotation" $ do
      check
        s
        """
        {-# language RoleAnnotations #-}
        data R a = MkR
        type role R nominal
        """
        [ ("/items/0/value/name", "\"R\""),
          ("/items/0/value/kind/type", "\"DataType\""),
          ("/items/1/value/kind/type", "\"DataConstructor\""),
          ("/items/2/value/name", "\"R\""),
          ("/items/2/value/kind/type", "\"RoleAnnotation\""),
          ("/items/2/value/parentKey", "0"),
          ("/items/2/value/signature", "\"nominal\"")
        ]

    Spec.it s "role annotation with multiple roles" $ do
      check
        s
        """
        {-# language RoleAnnotations #-}
        data T a b = MkT
        type role T nominal phantom
        """
        [ ("/items/0/value/name", "\"T\""),
          ("/items/0/value/kind/type", "\"DataType\""),
          ("/items/1/value/kind/type", "\"DataConstructor\""),
          ("/items/2/value/name", "\"T\""),
          ("/items/2/value/kind/type", "\"RoleAnnotation\""),
          ("/items/2/value/parentKey", "0"),
          ("/items/2/value/signature", "\"nominal phantom\"")
        ]

    Spec.it s "role annotation with name collision data T = T" $ do
      check
        s
        """
        {-# language RoleAnnotations #-}
        data T = T
        type role T nominal
        """
        [ ("/items/0/value/name", "\"T\""),
          ("/items/0/value/kind/type", "\"DataType\""),
          ("/items/0/value/key", "0"),
          ("/items/1/value/kind/type", "\"DataConstructor\""),
          ("/items/1/value/name", "\"T\""),
          ("/items/2/value/name", "\"T\""),
          ("/items/2/value/kind/type", "\"RoleAnnotation\""),
          ("/items/2/value/parentKey", "0"),
          ("/items/2/value/signature", "\"nominal\"")
        ]

    Spec.it s "orphaned role annotation has no parent" $ do
      check
        s
        """
        {-# language RoleAnnotations #-}
        type role R nominal
        """
        [ ("/items/0/value/name", "\"R\""),
          ("/items/0/value/kind/type", "\"RoleAnnotation\""),
          ("/items/0/value/parentKey", ""),
          ("/items/0/value/signature", "\"nominal\"")
        ]

  Spec.describe s "arguments" $ do
    Spec.it s "function with per-argument docs" $ do
      check
        s
        """
        f :: a -- ^ i
          -> a -- ^ o
        """
        [ ("/items/0/value/kind/type", "\"Function\""),
          ("/items/0/value/name", "\"f\""),
          ("/items/0/value/signature", "\"a -> a\""),
          ("/items/0/value/documentation/type", "\"Empty\""),
          ("/items/1/value/kind/type", "\"Argument\""),
          ("/items/1/value/parentKey", "0"),
          ("/items/1/value/signature", "\"a\""),
          ("/items/1/value/documentation/type", "\"Paragraph\""),
          ("/items/1/value/documentation/value/value", "\"i\""),
          ("/items/2/value/kind/type", "\"ReturnType\""),
          ("/items/2/value/parentKey", "0"),
          ("/items/2/value/signature", "\"a\""),
          ("/items/2/value/documentation/type", "\"Paragraph\""),
          ("/items/2/value/documentation/value/value", "\"o\"")
        ]

    Spec.it s "function without arg docs" $ do
      check
        s
        "f :: Int -> Bool -> String"
        [ ("/items/0/value/kind/type", "\"Function\""),
          ("/items/0/value/name", "\"f\""),
          ("/items/0/value/signature", "\"Int -> Bool -> String\"")
        ]

    Spec.it s "function with forall and constraints and arg docs" $ do
      check
        s
        """
        {-# language ExplicitForAll #-}
        f :: forall a. Show a => a -- ^ input
          -> String -- ^ output
        """
        [ ("/items/0/value/kind/type", "\"Function\""),
          ("/items/0/value/name", "\"f\""),
          ("/items/0/value/signature", "\"forall a. Show a =>\\n          a -> String\""),
          ("/items/1/value/kind/type", "\"Argument\""),
          ("/items/1/value/parentKey", "0"),
          ("/items/1/value/signature", "\"a\""),
          ("/items/1/value/documentation/type", "\"Paragraph\""),
          ("/items/1/value/documentation/value/value", "\"input\""),
          ("/items/2/value/kind/type", "\"ReturnType\""),
          ("/items/2/value/parentKey", "0"),
          ("/items/2/value/signature", "\"String\""),
          ("/items/2/value/documentation/type", "\"Paragraph\""),
          ("/items/2/value/documentation/value/value", "\"output\"")
        ]

    Spec.it s "function with return value doc only" $ do
      check
        s
        """
        f :: a
          -> a -- ^ lost
        """
        [ ("/items/0/value/kind/type", "\"Function\""),
          ("/items/0/value/name", "\"f\""),
          ("/items/0/value/signature", "\"a -> a\""),
          ("/items/0/value/documentation/type", "\"Empty\""),
          ("/items/1/value/kind/type", "\"ReturnType\""),
          ("/items/1/value/parentKey", "0"),
          ("/items/1/value/signature", "\"a\""),
          ("/items/1/value/documentation/type", "\"Paragraph\""),
          ("/items/1/value/documentation/value/value", "\"lost\"")
        ]

    Spec.it s "data constructor with arg doc has argument children" $ do
      check
        s
        """
        data T2
          = C2
            Int -- ^ arg doc
            Bool
        """
        [ ("/items/0/value/kind/type", "\"DataType\""),
          ("/items/0/value/name", "\"T2\""),
          ("/items/1/value/kind/type", "\"DataConstructor\""),
          ("/items/1/value/name", "\"C2\""),
          ("/items/1/value/signature", "\"Int -> Bool -> T2\""),
          ("/items/2/value/kind/type", "\"Argument\""),
          ("/items/2/value/parentKey", "1"),
          ("/items/2/value/signature", "\"Int\""),
          ("/items/2/value/documentation/type", "\"Paragraph\""),
          ("/items/2/value/documentation/value/value", "\"arg doc\""),
          ("/items/3/value/kind/type", "\"Argument\""),
          ("/items/3/value/parentKey", "1"),
          ("/items/3/value/signature", "\"Bool\""),
          ("/items/3/value/documentation/type", "\"Empty\"")
        ]

    Spec.it s "GADT constructor with arg doc has argument children" $ do
      check
        s
        """
        data T3 where
          C3 ::
            Int -- ^ arg doc
            -> T3
        """
        [ ("/items/0/value/kind/type", "\"DataType\""),
          ("/items/0/value/name", "\"T3\""),
          ("/items/1/value/kind/type", "\"GADTConstructor\""),
          ("/items/1/value/name", "\"C3\""),
          ("/items/1/value/signature", "\"Int -> T3\""),
          ("/items/2/value/kind/type", "\"Argument\""),
          ("/items/2/value/parentKey", "1"),
          ("/items/2/value/signature", "\"Int\""),
          ("/items/2/value/documentation/type", "\"Paragraph\""),
          ("/items/2/value/documentation/value/value", "\"arg doc\"")
        ]

    Spec.it s "class method with arg docs has argument children" $ do
      check
        s
        """
        class C a where
          m :: a -- ^ input
            -> Bool -- ^ result
            -> String
        """
        [ ("/items/0/value/kind/type", "\"Class\""),
          ("/items/0/value/name", "\"C a\""),
          ("/items/1/value/kind/type", "\"ClassMethod\""),
          ("/items/1/value/name", "\"m\""),
          ("/items/1/value/signature", "\"a -> Bool -> String\""),
          ("/items/2/value/kind/type", "\"Argument\""),
          ("/items/2/value/parentKey", "1"),
          ("/items/2/value/signature", "\"a\""),
          ("/items/2/value/documentation/type", "\"Paragraph\""),
          ("/items/2/value/documentation/value/value", "\"input\""),
          ("/items/3/value/kind/type", "\"Argument\""),
          ("/items/3/value/parentKey", "1"),
          ("/items/3/value/signature", "\"Bool\""),
          ("/items/3/value/documentation/type", "\"Paragraph\""),
          ("/items/3/value/documentation/value/value", "\"result\"")
        ]

  Spec.describe s "pragma combinations" $ do
    Spec.it s "inline and specialize on same function" $ do
      check
        s
        """
        f :: a -> a
        f = id
        {-# inline f #-}
        {-# specialize f :: () -> () #-}
        """
        [ ("/items/0/value/name", "\"f\""),
          ("/items/0/value/kind/type", "\"Function\""),
          ("/items/0/value/key", "0"),
          ("/items/1/value/name", "\"f\""),
          ("/items/1/value/kind/type", "\"InlineSignature\""),
          ("/items/1/value/parentKey", "0"),
          ("/items/2/value/name", "\"f\""),
          ("/items/2/value/kind/type", "\"SpecialiseSignature\""),
          ("/items/2/value/parentKey", "0")
        ]

    Spec.it s "warning and inline on same function" $ do
      check
        s
        """
        g :: a -> a
        g = id
        {-# warning g "deprecated" #-}
        {-# inline g #-}
        """
        [ ("/items/0/value/name", "\"g\""),
          ("/items/0/value/kind/type", "\"Function\""),
          ("/items/0/value/key", "0"),
          ("/items/1/value/name", "\"g\""),
          ("/items/1/value/kind/type", "\"Warning\""),
          ("/items/1/value/parentKey", "0"),
          ("/items/2/value/name", "\"g\""),
          ("/items/2/value/kind/type", "\"InlineSignature\""),
          ("/items/2/value/parentKey", "0")
        ]

    Spec.it s "multiple specialize pragmas on one function" $ do
      check
        s
        """
        h :: a -> a
        h = id
        {-# specialize h :: () -> () #-}
        {-# specialize h :: Int -> Int #-}
        """
        [ ("/items/0/value/name", "\"h\""),
          ("/items/0/value/kind/type", "\"Function\""),
          ("/items/1/value/name", "\"h\""),
          ("/items/1/value/kind/type", "\"SpecialiseSignature\""),
          ("/items/1/value/parentKey", "0"),
          ("/items/1/value/signature", "\"() -> ()\""),
          ("/items/2/value/name", "\"h\""),
          ("/items/2/value/kind/type", "\"SpecialiseSignature\""),
          ("/items/2/value/parentKey", "0"),
          ("/items/2/value/signature", "\"Int -> Int\"")
        ]

    Spec.it s "fixity and inline and specialize on same operator" $ do
      check
        s
        """
        (%) :: a -> a -> a
        (%) = const
        infixl 5 %
        {-# inline (%) #-}
        {-# specialize (%) :: () -> () -> () #-}
        """
        [ ("/items/0/value/name", "\"%\""),
          ("/items/0/value/kind/type", "\"Operator\""),
          ("/items/0/value/key", "0"),
          ("/items/1/value/name", "\"%\""),
          ("/items/1/value/kind/type", "\"FixitySignature\""),
          ("/items/1/value/parentKey", "0"),
          ("/items/2/value/name", "\"%\""),
          ("/items/2/value/kind/type", "\"InlineSignature\""),
          ("/items/2/value/parentKey", "0"),
          ("/items/3/value/name", "\"%\""),
          ("/items/3/value/kind/type", "\"SpecialiseSignature\""),
          ("/items/3/value/parentKey", "0")
        ]

  Spec.describe s "html" $ do
    Spec.it s "generates html without error" $ do
      checkHtml
        s
        []
        """
        -- | Module documentation.
        module M
          ( -- * Section
            x
          ) where

        import Data.List

        -- | A function.
        x :: Int
        x = 0

        -- | A data type.
        data T = C { field :: Bool }
        """

  Spec.describe s "cpp" $ do
    Spec.it s "works with simple ifdef" $ do
      check
        s
        """
        {-# language CPP #-}
        #define MY_FLAG
        #ifdef MY_FLAG
        module M where
        #endif
        """
        [("/name/value", "\"M\"")]

    Spec.it s "works with undefined macro" $ do
      check
        s
        """
        {-# language CPP #-}
        #ifdef UNDEFINED
        module M where
        #endif
        """
        [("/name", "")]

    Spec.it s "preserves line numbers" $ do
      check
        s
        """
        {-# language CPP #-}
        #ifdef FOO
        import Fake
        #endif
        x = 0
        """
        [("/items/0/location/line", "5")]

    Spec.it s "works with elif" $ do
      check
        s
        """
        {-# language CPP #-}
        #if 0
        module A where
        #elif 1
        module B where
        #endif
        """
        [("/name/value", "\"B\"")]

    Spec.it s "skips elif after match" $ do
      check
        s
        """
        {-# language CPP #-}
        #if 1
        module A where
        #elif 1
        module B where
        #endif
        """
        [("/name/value", "\"A\"")]

    Spec.it s "works with else after active ifdef" $ do
      check
        s
        """
        {-# language CPP #-}
        #define X
        #ifdef X
        module A where
        #else
        module B where
        #endif
        """
        [("/name/value", "\"A\"")]

    Spec.it s "uses else branch for undefined macros" $ do
      check
        s
        """
        {-# language CPP #-}
        #ifdef __GLASGOW_HASKELL__
        module GHC where
        #else
        module Other where
        #endif
        """
        [("/name/value", "\"Other\"")]

  Spec.describe s "literate" $ do
    Spec.describe s "bird" $ do
      Spec.it s "works" $ do
        checkWith
          s
          ["--literate"]
          "> x = 0"
          [("/items/0/value/name", "\"x\"")]

      Spec.it s "preserves line numbers" $ do
        checkWith
          s
          ["--literate"]
          """
          comment

          > x = 0
          """
          [("/items/0/location/line", "3")]

    Spec.describe s "latex" $ do
      Spec.it s "works" $ do
        checkWith
          s
          ["--literate"]
          """
          \\begin{code}
          x = 0
          \\end{code}
          """
          [("/items/0/value/name", "\"x\"")]

      Spec.it s "preserves line numbers" $ do
        checkWith
          s
          ["--literate"]
          """
          \\begin{code}
          x = 0
          \\end{code}
          """
          [("/items/0/location/line", "2")]

    Spec.it s "works with bird then latex" $ do
      checkWith
        s
        ["--literate"]
        """
        > x = 0

        \\begin{code}
          y = 1
        \\end{code}
        """
        [ ("/items/0/value/name", "\"x\""),
          ("/items/1/value/name", "\"y\"")
        ]

    Spec.it s "works with latex then bird" $ do
      checkWith
        s
        ["--literate"]
        """
        \\begin{code}
          x = 0
        \\end{code}

        > y = 1
        """
        [ ("/items/0/value/name", "\"x\""),
          ("/items/1/value/name", "\"y\"")
        ]

  Spec.describe s "signature" $ do
    Spec.it s "works with a simple signature" $ do
      checkWith
        s
        ["--signature"]
        "signature Foo where"
        [ ("/name/value", "\"Foo\""),
          ("/signature", "true")
        ]

    Spec.it s "works with a type signature" $ do
      checkWith
        s
        ["--signature"]
        """
        signature Foo where
          foo :: Int
        """
        [ ("/name/value", "\"Foo\""),
          ("/signature", "true"),
          ("/items/0/value/kind/type", "\"Function\""),
          ("/items/0/value/name", "\"foo\""),
          ("/items/0/value/signature", "\"Int\"")
        ]

    Spec.it s "works with an abstract data type" $ do
      checkWith
        s
        ["--signature"]
        """
        signature Foo where
          data T
        """
        [ ("/signature", "true"),
          ("/items/0/value/kind/type", "\"DataType\""),
          ("/items/0/value/name", "\"T\"")
        ]

    Spec.it s "works with a class" $ do
      checkWith
        s
        ["--signature"]
        """
        signature Foo where
          class C a where
            bar :: a -> Bool
        """
        [ ("/signature", "true"),
          ("/items/0/value/kind/type", "\"Class\""),
          ("/items/0/value/name", "\"C a\""),
          ("/items/1/value/kind/type", "\"ClassMethod\""),
          ("/items/1/value/name", "\"bar\"")
        ]

    Spec.it s "works with documentation" $ do
      checkWith
        s
        ["--signature"]
        """
        -- | Module doc
        signature Foo where
        """
        [ ("/signature", "true"),
          ("/documentation/type", "\"Paragraph\""),
          ("/documentation/value/type", "\"String\""),
          ("/documentation/value/value", "\"Module doc\"")
        ]

  Spec.describe s "argument names" $ do
    Spec.it s "captures argument name from binding" $ do
      check
        s
        """
        f :: a {- ^ doc -} -> a
        f x = x
        """
        [ ("/items/0/value/kind/type", "\"Function\""),
          ("/items/0/value/name", "\"f\""),
          ("/items/1/value/kind/type", "\"Argument\""),
          ("/items/1/value/name", "\"x\"")
        ]

    Spec.it s "picks first variable name across equations" $ do
      check
        s
        """
        or :: Bool {- ^ doc -} -> Bool -> Bool
        or True _ = True
        or _ x = x
        """
        [ ("/items/0/value/name", "\"or\""),
          ("/items/1/value/kind/type", "\"Argument\""),
          ("/items/1/value/name", ""),
          ("/items/2/value/kind/type", "\"Argument\""),
          ("/items/2/value/name", "\"x\"")
        ]

    Spec.it s "handles all-wildcard arguments" $ do
      check
        s
        """
        f :: a {- ^ doc -} -> a
        f _ = undefined
        """
        [ ("/items/1/value/kind/type", "\"Argument\""),
          ("/items/1/value/name", "")
        ]

    Spec.it s "handles signature without binding" $ do
      check
        s
        """
        f :: a {- ^ doc -} -> a
        """
        [ ("/items/1/value/kind/type", "\"Argument\""),
          ("/items/1/value/name", "")
        ]

  Spec.describe s "visibility" $ do
    Spec.it s "marks exported items as Exported" $ do
      check
        s
        """
        module M ( x ) where
        x = ()
        y = ()
        """
        [ ("/items/0/value/visibility/type", "\"Exported\""),
          ("/items/1/value/visibility/type", "\"Unexported\"")
        ]

    Spec.it s "marks all items as Exported when no export list" $ do
      check
        s
        """
        x = ()
        y = ()
        """
        [ ("/items/0/value/visibility/type", "\"Exported\""),
          ("/items/1/value/visibility/type", "\"Exported\"")
        ]

    Spec.it s "marks class instances as Implicit" $ do
      check
        s
        """
        module M ( MyClass ) where
        class MyClass a
        instance MyClass Int
        """
        [ ("/items/0/value/visibility/type", "\"Exported\""),
          ("/items/1/value/visibility/type", "\"Implicit\"")
        ]

    Spec.it s "marks rules as Implicit" $ do
      check
        s
        """
        module M ( f ) where
        f x = x
        {-# RULES "f/id" f = id #-}
        """
        [ ("/items/0/value/visibility/type", "\"Exported\""),
          ("/items/1/value/visibility/type", "\"Implicit\"")
        ]

    Spec.it s "marks constructors as Unexported when type has no subordinates" $ do
      check
        s
        """
        module M ( T ) where
        data T = C1 | C2
        """
        [ ("/items/0/value/visibility/type", "\"Exported\""),
          ("/items/1/value/visibility/type", "\"Unexported\""),
          ("/items/2/value/visibility/type", "\"Unexported\"")
        ]

    Spec.it s "marks all constructors as Exported with wildcard subordinates" $ do
      check
        s
        """
        module M ( T(..) ) where
        data T = C1 | C2
        """
        [ ("/items/0/value/visibility/type", "\"Exported\""),
          ("/items/1/value/visibility/type", "\"Exported\""),
          ("/items/2/value/visibility/type", "\"Exported\"")
        ]

    Spec.it s "marks only listed constructors as Exported with explicit subordinates" $ do
      check
        s
        """
        module M ( T(C1) ) where
        data T = C1 | C2
        """
        [ ("/items/0/value/visibility/type", "\"Exported\""),
          ("/items/1/value/visibility/type", "\"Exported\""),
          ("/items/2/value/visibility/type", "\"Unexported\"")
        ]

    Spec.it s "marks methods as Unexported when class has no subordinates" $ do
      check
        s
        """
        module M ( MyClass ) where
        class MyClass a where
          myMethod :: a -> a
        """
        [ ("/items/0/value/visibility/type", "\"Exported\""),
          ("/items/1/value/visibility/type", "\"Unexported\"")
        ]

    Spec.it s "marks all methods as Exported with wildcard subordinates" $ do
      check
        s
        """
        module M ( MyClass(..) ) where
        class MyClass a where
          myMethod :: a -> a
        """
        [ ("/items/0/value/visibility/type", "\"Exported\""),
          ("/items/1/value/visibility/type", "\"Exported\"")
        ]

    Spec.it s "pattern synonyms with COMPLETE pragma are not duplicated in exports" $ do
      check
        s
        """
        {-# language PatternSynonyms #-}
        module M (Nil, Cons) where
        pattern Nil = []
        pattern Cons x xs = x : xs
        {-# complete Nil, Cons #-}
        """
        [ ("/items/0/value/kind/type", "\"PatternSynonym\""),
          ("/items/0/value/name", "\"Nil\""),
          ("/items/1/value/kind/type", "\"PatternSynonym\""),
          ("/items/1/value/name", "\"Cons\""),
          ("/items/2/value/kind/type", "\"CompletePragma\"")
        ]

-- | Run the pipeline on the given Haskell source and assert JSON pointer
-- expectations. Each @(pointer, json)@ pair asserts that the value at
-- @pointer@ equals the parsed @json@. Use an empty string for @json@
-- (or the 'checkAbsent' wrapper) to assert that the pointer does not
-- resolve to any value.
check :: (Stack.HasCallStack, Monad m) => Spec.Spec m n -> String -> [(String, String)] -> m ()
check s = checkWith s []

-- | Assert that a JSON pointer path does not resolve to any value.
-- Equivalent to @(pointer, "")@ in a 'check' assertion list.
checkAbsent :: String -> (String, String)
checkAbsent pointer = (pointer, "")

checkWith :: (Stack.HasCallStack, Monad m) => Spec.Spec m n -> [String] -> String -> [(String, String)] -> m ()
checkWith s arguments input assertions = do
  result <-
    either (Spec.assertFailure s . Exception.displayException) pure
      . Main.mainWith "scrod-test-suite" arguments
      $ pure input
  module_ <- either (Spec.assertFailure s) pure result
  json <-
    maybe (Spec.assertFailure s "impossible") pure
      . Parsec.parseString Json.decode
      $ Builder.toString module_

  Monad.forM_ assertions $ \(p, j) -> do
    pointer <- maybe (Spec.assertFailure s "invalid pointer") pure $ Parsec.parseString Pointer.decode p
    expected <-
      if null j
        then pure Nothing
        else maybe (Spec.assertFailure s "invalid json") (pure . Just) $ Parsec.parseString Json.decode j
    let actual = Pointer.evaluate pointer json
    Monad.unless (actual == expected)
      . Spec.assertFailure s
      $ List.intercalate
        "\n"
        [ "at " <> p,
          "expected " <> maybe "(nothing)" (Builder.toString . Json.encode) expected,
          " but got " <> maybe "(nothing)" (Builder.toString . Json.encode) actual
        ]

checkHtml :: (Stack.HasCallStack, Monad m) => Spec.Spec m n -> [String] -> String -> m ()
checkHtml s arguments input = do
  result <-
    either (Spec.assertFailure s . Exception.displayException) pure
      . Main.mainWith "scrod-test-suite" ("--format" : "html" : arguments)
      $ pure input
  output <- either (Spec.assertFailure s) pure result
  Monad.when (null $ Builder.toString output) $
    Spec.assertFailure s "expected non-empty HTML output"
