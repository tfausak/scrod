{-# LANGUAGE MultilineStrings #-}
{-# OPTIONS_GHC -O0 #-}

module Scrod.TestSuite.Integration where

import qualified Control.Monad as Monad
import qualified Data.List as List
import qualified GHC.Stack as Stack
import qualified Scrod.Convert.FromGhc as FromGhc
import qualified Scrod.Convert.ToJson as ToJson
import qualified Scrod.Extra.Builder as Builder
import qualified Scrod.Extra.Parsec as Parsec
import qualified Scrod.Ghc.Parse as Parse
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
      [ ("/language", "null"),
        ("/extensions", "{}"),
        ("/documentation/type", "\"Empty\""),
        ("/documentation/value", "null"),
        ("/since", "null"),
        ("/name", "null"),
        ("/warning", "null"),
        ("/exports", "null"),
        ("/items", "[]")
      ]

  Spec.describe s "version" $ do
    Spec.it s "works" $ do
      -- Note that we don't want this test to be too specific, otherwise we'd
      -- have to update it every time we change the version number. So instead
      -- we just check the first component.
      check s "" [("/version/0", "0")]

  Spec.describe s "language" $ do
    Spec.it s "defaults to null" $ do
      check s "" [("/language", "null")]

    Spec.it s "works with a language" $ do
      check s "{-# language Haskell98 #-}" [("/language", "\"Haskell98\"")]

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
          ("/documentation/value/value/namespace", "null"),
          ("/documentation/value/value/value", "\"x\"")
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
          ("/documentation/value/value/namespace", "\"Value\""),
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
          ("/documentation/value/value/namespace", "\"Type\""),
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
          ("/documentation/value/value/name", "\"X\""),
          ("/documentation/value/value/label", "null")
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
          ("/documentation/value/value/url", "\"http://example\""),
          ("/documentation/value/value/label", "null")
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
          ("/documentation/value/value/label/value/url", "\"http://example\""),
          ("/documentation/value/value/label/value/label", "null")
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
    Spec.it s "defaults to null" $ do
      check s "" [("/since", "null")]

    Spec.it s "" $ do
      -- TODO: Needs tests once `extractModuleSince` is implemented.
      pure ()

  Spec.describe s "name" $ do
    Spec.it s "defaults to null" $ do
      check s "" [("/name", "null")]

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
    Spec.it s "defaults to null" $ do
      check s "" [("/warning", "null")]

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
    Spec.it s "defaults to null" $ do
      check s "" [("/exports", "null")]

    Spec.it s "works with an empty list" $ do
      check s "module M () where" [("/exports", "[]")]

    Spec.describe s "identifier" $ do
      Spec.it s "works" $ do
        check
          s
          "module M ( pi ) where"
          [ ("/exports/0/type", "\"Identifier\""),
            ("/exports/0/value/name/kind", "null"),
            ("/exports/0/value/name/name", "\"pi\""),
            ("/exports/0/value/subordinates", "null"),
            ("/exports/0/value/warning", "null"),
            ("/exports/0/value/doc", "null")
          ]

      Spec.it s "works with an explicit pattern" $ do
        check
          s
          """
          {-# language PatternSynonyms #-}
          module M ( pattern True ) where
          """
          [ ("/exports/0/type", "\"Identifier\""),
            ("/exports/0/value/name/kind", "\"Pattern\""),
            ("/exports/0/value/name/name", "\"True\""),
            ("/exports/0/value/subordinates", "null"),
            ("/exports/0/value/warning", "null"),
            ("/exports/0/value/doc", "null")
          ]

      Spec.it s "works with an explicit type" $ do
        check
          s
          """
          {-# language ExplicitNamespaces #-}
          module M ( type Bool ) where
          """
          [ ("/exports/0/type", "\"Identifier\""),
            ("/exports/0/value/name/kind", "\"Type\""),
            ("/exports/0/value/name/name", "\"Bool\""),
            ("/exports/0/value/subordinates", "null"),
            ("/exports/0/value/warning", "null"),
            ("/exports/0/value/doc", "null")
          ]

      Spec.it s "works with a module" $ do
        check
          s
          "module M ( module M ) where"
          [ ("/exports/0/type", "\"Identifier\""),
            ("/exports/0/value/name/kind", "\"Module\""),
            ("/exports/0/value/name/name", "\"M\""),
            ("/exports/0/value/subordinates", "null"),
            ("/exports/0/value/warning", "null"),
            ("/exports/0/value/doc", "null")
          ]

      Spec.it s "works with a subordinate" $ do
        check
          s
          "module M ( Bool ( True ) ) where"
          [ ("/exports/0/type", "\"Identifier\""),
            ("/exports/0/value/name/kind", "null"),
            ("/exports/0/value/name/name", "\"Bool\""),
            ("/exports/0/value/subordinates/wildcard", "false"),
            ("/exports/0/value/subordinates/explicit/0/kind", "null"),
            ("/exports/0/value/subordinates/explicit/0/name", "\"True\""),
            ("/exports/0/value/warning", "null"),
            ("/exports/0/value/doc", "null")
          ]

      Spec.it s "works with a wildcard" $ do
        check
          s
          "module M ( Bool ( .. ) ) where"
          [ ("/exports/0/type", "\"Identifier\""),
            ("/exports/0/value/name/kind", "null"),
            ("/exports/0/value/name/name", "\"Bool\""),
            ("/exports/0/value/subordinates/wildcard", "true"),
            ("/exports/0/value/subordinates/explicit", "[]"),
            ("/exports/0/value/warning", "null"),
            ("/exports/0/value/doc", "null")
          ]

      Spec.it s "works with a warning" $ do
        check
          s
          """
          module M ( {-# warning "foo" #-} pi ) where
          """
          [ ("/exports/0/type", "\"Identifier\""),
            ("/exports/0/value/name/kind", "null"),
            ("/exports/0/value/name/name", "\"pi\""),
            ("/exports/0/value/subordinates", "null"),
            ("/exports/0/value/warning/category", "\"deprecations\""),
            ("/exports/0/value/warning/value", "\"foo\""),
            ("/exports/0/value/doc", "null")
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
            ("/exports/0/value/name/kind", "null"),
            ("/exports/0/value/name/name", "\"pi\""),
            ("/exports/0/value/subordinates", "null"),
            ("/exports/0/value/warning", "null"),
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
            ("/items/0/value/kind", "\"Function\""),
            ("/items/0/value/parentKey", "null"),
            ("/items/0/value/name", "\"x\""),
            ("/items/0/value/documentation/type", "\"Empty\""),
            ("/items/0/value/signature", "null")
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
            ("/items/0/value/signature", "\"x :: Int\"")
          ]

    do
      -- TODO: The tests in this section are pretty bare bones. They could be
      -- expanded to check more properties and more variants. Each `it` could
      -- probably become a `describe` with multiple tests for each item kind.
      -- Also the input parses but might not compile. It would be nice to have
      -- input that actually compiles.

      Spec.it s "open type family" $ do
        check s "type family A" [("/items/0/value/kind", "\"OpenTypeFamily\"")]

      Spec.it s "closed type family" $ do
        check s "type family B where" [("/items/0/value/kind", "\"ClosedTypeFamily\"")]

      Spec.it s "closed type family with instance" $ do
        -- TODO: This data instance should create an item.
        check s "type family C where D = E" [("/items/0/value/kind", "\"ClosedTypeFamily\"")]

      Spec.it s "data family" $ do
        check s "data family F" [("/items/0/value/kind", "\"DataFamily\"")]

      Spec.it s "type synonym" $ do
        check s "type G = ()" [("/items/0/value/kind", "\"TypeSynonym\"")]

      Spec.it s "data" $ do
        check s "data H" [("/items/0/value/kind", "\"DataType\"")]

      Spec.it s "data constructor" $ do
        check
          s
          "data I = J"
          [ ("/items/0/value/kind", "\"DataType\""),
            ("/items/1/value/kind", "\"DataConstructor\"")
          ]

      Spec.it s "data constructor GADT" $ do
        check
          s
          "data K where L :: K"
          [ ("/items/0/value/kind", "\"DataType\""),
            ("/items/1/value/kind", "\"GADTConstructor\"")
          ]

      Spec.it s "type data" $ do
        check s "type data L" [("/items/0/value/kind", "\"TypeData\"")]

      Spec.it s "type data constructor" $ do
        check
          s
          "type data M = N"
          [ ("/items/0/value/kind", "\"TypeData\""),
            ("/items/1/value/kind", "\"DataConstructor\"")
          ]

      Spec.it s "type data constructor GADT" $ do
        check
          s
          "type data O where P :: O"
          [ ("/items/0/value/kind", "\"TypeData\""),
            ("/items/1/value/kind", "\"GADTConstructor\"")
          ]

      Spec.it s "newtype" $ do
        check
          s
          "newtype Q = R ()"
          [ ("/items/0/value/kind", "\"Newtype\""),
            ("/items/1/value/kind", "\"DataConstructor\"")
          ]

      Spec.it s "record field" $ do
        check
          s
          "data S = T { u :: () }"
          [ ("/items/0/value/kind", "\"DataType\""),
            ("/items/1/value/kind", "\"DataConstructor\""),
            ("/items/2/value/kind", "\"RecordField\"")
          ]

      Spec.it s "record field GADT" $ do
        check
          s
          "data V where W :: { x :: () } -> V"
          [ ("/items/0/value/kind", "\"DataType\""),
            ("/items/1/value/kind", "\"GADTConstructor\""),
            ("/items/2/value/kind", "\"RecordField\"")
          ]

      Spec.it s "class" $ do
        check s "class Y" [("/items/0/value/kind", "\"Class\"")]

      Spec.it s "class instance" $ do
        check s "instance Z" [("/items/0/value/kind", "\"ClassInstance\"")]

      Spec.it s "data instance" $ do
        check s "data instance A" [("/items/0/value/kind", "\"DataFamilyInstance\"")]

      Spec.it s "data instance constructor" $ do
        -- TODO: This constructor should create an item.
        check s "data instance B = C" [("/items/0/value/kind", "\"DataFamilyInstance\"")]

      Spec.it s "data instance constructor GADT" $ do
        -- TODO: This constructor should create an item.
        check s "data instance D where E :: D" [("/items/0/value/kind", "\"DataFamilyInstance\"")]

      Spec.it s "newtype instance" $ do
        -- TODO: This constructor should create an item.
        check s "newtype instance F = G" [("/items/0/value/kind", "\"DataFamilyInstance\"")]

      Spec.it s "newtype instance GADT" $ do
        -- TODO: This constructor should create an item.
        check s "newtype instance H where I :: H" [("/items/0/value/kind", "\"DataFamilyInstance\"")]

      Spec.it s "type instance" $ do
        check s "type instance J = K" [("/items/0/value/kind", "\"TypeFamilyInstance\"")]

      Spec.it s "standalone deriving" $ do
        check s "deriving instance L" [("/items/0/value/kind", "\"StandaloneDeriving\"")]

      Spec.it s "standalone deriving stock" $ do
        check s "deriving stock instance M" [("/items/0/value/kind", "\"StandaloneDeriving\"")]

      Spec.it s "standalone deriving newtype" $ do
        check s "deriving newtype instance N" [("/items/0/value/kind", "\"StandaloneDeriving\"")]

      Spec.it s "standalone deriving anyclass" $ do
        check s "deriving anyclass instance O" [("/items/0/value/kind", "\"StandaloneDeriving\"")]

      Spec.it s "standalone deriving via" $ do
        check s "deriving via P instance Q" [("/items/0/value/kind", "\"StandaloneDeriving\"")]

      Spec.it s "data deriving" $ do
        check
          s
          "data R deriving S"
          [ ("/items/0/value/kind", "\"DataType\""),
            ("/items/1/value/kind", "\"DerivedInstance\"")
          ]

      Spec.it s "data GADT deriving" $ do
        check s "data T where deriving U" []

      Spec.it s "type data deriving" $ do
        check s "type data V deriving W" []

      Spec.it s "type data GADT deriving" $ do
        check s "type data X where deriving Y" []

      Spec.it s "newtype deriving" $ do
        check s "newtype Z = A deriving B" []

      Spec.it s "newtype GADT deriving" $ do
        check s "newtype C where D :: C deriving D" []

      Spec.it s "function" $ do
        check s "e f = ()" []

      Spec.it s "infix function" $ do
        check s "g `h` i = ()" []

      Spec.it s "infix operator" $ do
        check s "j % k = ()" []

      Spec.it s "prefix operator" $ do
        check s "(&) = ()" []

      Spec.it s "strict variable" $ do
        check s "!l = ()" []

      Spec.it s "lazy pattern" $ do
        check s "~m = ()" []

      Spec.it s "as pattern" $ do
        check s "n@o = ()" []

      Spec.it s "parenthesized pattern" $ do
        check s "(p) = ()" []

      Spec.it s "bang pattern" $ do
        check s "(!q) = ()" []

      Spec.it s "list pattern" $ do
        check s "[r] = ()" []

      Spec.it s "tuple pattern" $ do
        check s "(s, t) = ()" []

      Spec.it s "anonymous sum pattern" $ do
        check s "{-# language UnboxedSums #-} (# u | #) = ()" []

      Spec.it s "prefix constructor pattern" $ do
        check s "Just v = ()" []

      Spec.it s "record constructor pattern" $ do
        check s "W { x = y } = ()" []

      Spec.it s "punned record pattern" $ do
        check s "Z { a } = ()" []

      Spec.it s "wild card record pattern" $ do
        check s "B { .. } = ()" []

      Spec.it s "infix constructor pattern" $ do
        check s "(c : d) = ()" []

      Spec.it s "view pattern" $ do
        check s "(e -> f) = ()" []

      Spec.it s "splice pattern" $ do
        check s "{-# language TemplateHaskell #-} $( g ) = ()" []

      Spec.it s "literal pattern" $ do
        check s "'h' = ()" []

      Spec.it s "natural pattern" $ do
        check s "0 = ()" []

      Spec.it s "n+k pattern" $ do
        check s "{-# language NPlusKPatterns #-} (i + 1) = ()" []

      Spec.it s "signature pattern" $ do
        check s "(j :: K) = ()" []

      Spec.it s "bidirectional pattern synonym" $ do
        check s "{-# language PatternSynonyms #-} pattern L = ()" []

      Spec.it s "unidirectional pattern synonym" $ do
        check s "{-# language PatternSynonyms #-} pattern M <- ()" []

      Spec.it s "explicitly bidirectional pattern synonym" $ do
        check s "{-# language PatternSynonyms #-} pattern N <- () where N = ()" []

      Spec.it s "type signature" $ do
        check s "o :: ()" []

      Spec.it s "pattern type signature" $ do
        check s "{-# language PatternSynonyms #-} pattern P :: ()" []

      Spec.describe s "method signature" $ do
        Spec.it s "works" $ do
          check
            s
            """
            class C a where
              m :: a
            """
            [ ("/items/0/value/key", "0"),
              ("/items/0/value/kind", "\"Class\""),
              ("/items/1/value/key", "1"),
              ("/items/1/value/kind", "\"ClassMethod\""),
              ("/items/1/value/parentKey", "0"),
              ("/items/1/value/signature", "\"m :: a\"")
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

      Spec.it s "default method signature" $ do
        check s "class S where default t :: ()" []

      Spec.it s "fixity" $ do
        check s "infixl 5 +" []

      Spec.it s "inline pragma" $ do
        check s "{-# inline i #-}" []

      Spec.it s "inline pragma with phase control" $ do
        check s "{-# inline [1] j #-}" []

      Spec.it s "inline pragma with inverted phase control" $ do
        check s "{-# inline [~2] k #-}" []

      Spec.it s "noinline pragma" $ do
        check s "{-# noinline l #-}" []

      Spec.it s "specialize pragma" $ do
        check s "{-# specialize j :: () #-}" []

      Spec.it s "specialize instance pragma" $ do
        check s "{-# specialize instance K #-}" []

      Spec.it s "minimal pragma" $ do
        check s "{-# minimal l #-}" []

      Spec.it s "set cost center pragma" $ do
        check s "{-# scc m #-}" []

      Spec.it s "complete pragma" $ do
        check s "{-# complete N #-}" []

      Spec.it s "standalone kind signature" $ do
        check s "type O :: ()" []

      Spec.it s "default declaration" $ do
        check s "default ()" []

      Spec.it s "foreign import" $ do
        check s "{-# language ForeignFunctionInterface #-} foreign import ccall \"\" p :: ()" []

      Spec.it s "warning pragma" $ do
        check s "{-# warning x \"\" #-}" []

      Spec.it s "value annotation" $ do
        check s "{-# ann x () #-}" []

      Spec.it s "type annotation" $ do
        check s "{-# ann type X () #-}" []

      Spec.it s "module annotation" $ do
        check s "{-# ann module () #-}" []

      Spec.it s "rules pragma" $ do
        check s "{-# rules \"q\" x = () #-}" []

      Spec.it s "splice declaration" $ do
        check s "{-# language TemplateHaskellQuotes #-} $( x )" []

      Spec.it s "role annotation" $ do
        check s "type role R nominal" []

check :: (Stack.HasCallStack, Monad m) => Spec.Spec m n -> String -> [(String, String)] -> m ()
check s input assertions = do
  parsed <- either (Spec.assertFailure s) pure $ Parse.parse input
  module_ <- either (Spec.assertFailure s) pure $ FromGhc.fromGhc parsed
  let json = ToJson.toJson module_
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
