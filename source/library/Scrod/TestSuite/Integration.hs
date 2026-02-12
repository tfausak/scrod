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
        ("/documentation/value", "null"),
        ("/imports", "[]"),
        ("/signature", "false"),
        ("/items", "[]")
      ]

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
          ("/documentation/value/0/0", "1"),
          ("/documentation/value/0/1/type", "\"Paragraph\""),
          ("/documentation/value/0/1/value/type", "\"String\""),
          ("/documentation/value/0/1/value/value", "\"x\"")
        ]

    Spec.it s "works with an ordered list using parens" $ do
      check
        s
        """
        -- | (1) x
        module M where
        """
        [ ("/documentation/type", "\"OrderedList\""),
          ("/documentation/value/0/0", "1"),
          ("/documentation/value/0/1/type", "\"Paragraph\""),
          ("/documentation/value/0/1/value/type", "\"String\""),
          ("/documentation/value/0/1/value/value", "\"x\"")
        ]

    Spec.it s "works with an ordered list starting at 2" $ do
      check
        s
        """
        -- | 2. x
        module M where
        """
        [ ("/documentation/type", "\"OrderedList\""),
          ("/documentation/value/0/0", "2"),
          ("/documentation/value/0/1/type", "\"Paragraph\""),
          ("/documentation/value/0/1/value/type", "\"String\""),
          ("/documentation/value/0/1/value/value", "\"x\"")
        ]

    Spec.it s "works with a definition list" $ do
      check
        s
        """
        -- | [x]: y
        module M where
        """
        [ ("/documentation/type", "\"DefList\""),
          ("/documentation/value/0/0/type", "\"String\""),
          ("/documentation/value/0/0/value", "\"x\""),
          ("/documentation/value/0/1/type", "\"String\""),
          ("/documentation/value/0/1/value", "\"y\"")
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
        -- |-------+   |
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
          ("/documentation/value/bodyRows/0/0/contents/value", "\"d\\n-------\\nf | g\""),
          ("/documentation/value/bodyRows/0/1/colspan", "1"),
          ("/documentation/value/bodyRows/0/1/rowspan", "1"),
          ("/documentation/value/bodyRows/0/1/contents/type", "\"String\""),
          ("/documentation/value/bodyRows/0/1/contents/value", "\"e\\n\\n\"")
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

  Spec.describe s "exports" $ do
    Spec.it s "defaults to absent" $ do
      check s "" [("/exports", "")]

    Spec.it s "works with an empty list" $ do
      check s "module M () where" [("/exports", "[]")]

    Spec.describe s "identifier" $ do
      Spec.it s "works" $ do
        check
          s
          "module M ( pi ) where"
          [ ("/exports/0/type", "\"Identifier\""),
            ("/exports/0/value/name/name", "\"pi\"")
          ]

      Spec.it s "works with an explicit pattern" $ do
        check
          s
          """
          {-# language PatternSynonyms #-}
          module M ( pattern True ) where
          """
          [ ("/exports/0/type", "\"Identifier\""),
            ("/exports/0/value/name/kind/type", "\"Pattern\""),
            ("/exports/0/value/name/name", "\"True\"")
          ]

      Spec.it s "works with an explicit type" $ do
        check
          s
          """
          {-# language ExplicitNamespaces #-}
          module M ( type Bool ) where
          """
          [ ("/exports/0/type", "\"Identifier\""),
            ("/exports/0/value/name/kind/type", "\"Type\""),
            ("/exports/0/value/name/name", "\"Bool\"")
          ]

      Spec.it s "works with a module" $ do
        check
          s
          "module M ( module M ) where"
          [ ("/exports/0/type", "\"Identifier\""),
            ("/exports/0/value/name/kind/type", "\"Module\""),
            ("/exports/0/value/name/name", "\"M\"")
          ]

      Spec.it s "works with a subordinate" $ do
        check
          s
          "module M ( Bool ( True ) ) where"
          [ ("/exports/0/type", "\"Identifier\""),
            ("/exports/0/value/name/name", "\"Bool\""),
            ("/exports/0/value/subordinates/wildcard", "false"),
            ("/exports/0/value/subordinates/explicit/0/name", "\"True\"")
          ]

      Spec.it s "works with a wildcard" $ do
        check
          s
          "module M ( Bool ( .. ) ) where"
          [ ("/exports/0/type", "\"Identifier\""),
            ("/exports/0/value/name/name", "\"Bool\""),
            ("/exports/0/value/subordinates/wildcard", "true"),
            ("/exports/0/value/subordinates/explicit", "[]")
          ]

      Spec.it s "works with a warning" $ do
        check
          s
          """
          module M ( {-# warning "foo" #-} pi ) where
          """
          [ ("/exports/0/type", "\"Identifier\""),
            ("/exports/0/value/name/name", "\"pi\""),
            ("/exports/0/value/warning/category", "\"deprecations\""),
            ("/exports/0/value/warning/value", "\"foo\"")
          ]

      Spec.it s "works with a doc" $ do
        check
          s
          """
          module M
            ( pi -- ^ foo
            ) where
          """
          [ ("/exports/0/type", "\"Identifier\""),
            ("/exports/0/value/name/name", "\"pi\""),
            ("/exports/0/value/doc/type", "\"Paragraph\""),
            ("/exports/0/value/doc/value/type", "\"String\""),
            ("/exports/0/value/doc/value/value", "\"foo\"")
          ]

    Spec.describe s "group" $ do
      Spec.it s "works" $ do
        check
          s
          """
          module M
            ( -- * foo
            ) where
          """
          [ ("/exports/0/type", "\"Group\""),
            ("/exports/0/value/level", "1"),
            ("/exports/0/value/title/type", "\"Paragraph\""),
            ("/exports/0/value/title/value/type", "\"String\""),
            ("/exports/0/value/title/value/value", "\"foo\"")
          ]

    Spec.describe s "doc" $ do
      Spec.it s "works" $ do
        check
          s
          """
          module M
            ( -- | foo
            ) where
          """
          [ ("/exports/0/type", "\"Doc\""),
            ("/exports/0/value/type", "\"Paragraph\""),
            ("/exports/0/value/value/type", "\"String\""),
            ("/exports/0/value/value/value", "\"foo\"")
          ]

    Spec.describe s "doc named" $ do
      Spec.it s "works" $ do
        check
          s
          """
          module M
            ( -- $foo
            ) where
          """
          [ ("/exports/0/type", "\"DocNamed\""),
            ("/exports/0/value", "\"foo\"")
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
          ("/items/1/value/parentKey", "0")
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
          ("/items/2/value/kind/type", "\"GADTConstructor\""),
          ("/items/2/value/name", "\"B\""),
          ("/items/2/value/signature", "\"Int -> T\"")
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
          ("/items/2/value/kind/type", "\"RecordField\"")
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
          ("/items/0/value/name", "\"C\""),
          ("/items/0/value/signature", "\"a\"")
        ]

    Spec.it s "class instance" $ do
      check
        s
        """
        class Z a
        instance Z ()
        """
        [("/items/1/value/kind/type", "\"ClassInstance\"")]

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
        [ ("/items/1/value/kind/type", "\"ClassInstance\"")
        ]

    Spec.it s "data instance" $ do
      check
        s
        """
        {-# language TypeFamilies #-}
        data family A a
        data instance A ()
        """
        [("/items/1/value/kind/type", "\"DataFamilyInstance\"")]

    Spec.it s "data instance constructor" $ do
      check
        s
        """
        {-# language TypeFamilies #-}
        data family B a
        data instance B () = C
        """
        [ ("/items/1/value/kind/type", "\"DataFamilyInstance\""),
          ("/items/2/value/kind/type", "\"DataConstructor\""),
          ("/items/2/value/parentKey", "1")
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
          ("/items/2/value/kind/type", "\"DataConstructor\""),
          ("/items/2/value/parentKey", "1")
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
        [("/items/1/value/kind/type", "\"TypeFamilyInstance\"")]

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
        [ ("/items/2/value/kind/type", "\"StandaloneDeriving\""),
          ("/items/2/value/name", "\"Show N\""),
          ("/items/2/value/parentKey", "0")
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
          ("/items/2/value/parentKey", "1")
        ]

    Spec.it s "standalone deriving via" $ do
      check
        s
        """
        {-# language DerivingStrategies, DerivingVia #-}
        newtype P = MkP Int
        deriving via Int instance Show P
        """
        [ ("/items/2/value/kind/type", "\"StandaloneDeriving\""),
          ("/items/2/value/name", "\"Show P\""),
          ("/items/2/value/parentKey", "0")
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
      check s "{-# language PatternSynonyms #-} pattern L = ()" []

    Spec.it s "unidirectional pattern synonym" $ do
      check s "{-# language PatternSynonyms #-} pattern M <- ()" []

    Spec.it s "explicitly bidirectional pattern synonym" $ do
      check s "{-# language PatternSynonyms #-} pattern N <- () where N = ()" []

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
        []

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
        []

    Spec.it s "fixity" $ do
      check
        s
        """
        infixl 5 %
        (%) :: () -> () -> ()
        (%) _ _ = ()
        """
        []

    Spec.it s "inline pragma" $ do
      check
        s
        """
        i = ()
        {-# inline i #-}
        """
        []

    Spec.it s "inline pragma with phase control" $ do
      check
        s
        """
        j = ()
        {-# inline [1] j #-}
        """
        []

    Spec.it s "inline pragma with inverted phase control" $ do
      check
        s
        """
        k = ()
        {-# inline [~2] k #-}
        """
        []

    Spec.it s "noinline pragma" $ do
      check
        s
        """
        l = ()
        {-# noinline l #-}
        """
        []

    Spec.it s "specialize pragma" $ do
      check
        s
        """
        j2 :: a -> a
        j2 = id
        {-# specialize j2 :: () -> () #-}
        """
        []

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
        []

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
        []

    Spec.it s "standalone kind signature" $ do
      check
        s
        """
        type O :: *
        data O
        """
        []

    Spec.it s "default declaration" $ do
      check s "default ()" []

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

    Spec.it s "warning pragma" $ do
      check
        s
        """
        x = ()
        {-# warning x "" #-}
        """
        []

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
        []

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
        []

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
          ("/items/0/value/name", "\"C\""),
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

check :: (Stack.HasCallStack, Monad m) => Spec.Spec m n -> String -> [(String, String)] -> m ()
check s = checkWith s []

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
