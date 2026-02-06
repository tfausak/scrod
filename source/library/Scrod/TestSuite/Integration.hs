{-# LANGUAGE MultilineStrings #-}
{-# OPTIONS_GHC -O0 #-}

module Scrod.TestSuite.Integration where

import qualified Control.Monad as Monad
import qualified GHC.Stack as Stack
import qualified Scrod.Convert.FromGhc as FromGhc
import qualified Scrod.Convert.ToJson as ToJson
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

  Spec.describe s "extensions" $ do
    Spec.it s "defaults to empty object" $ do
      check s "" [("/extensions", "{}")]

    Spec.it s "works with an enabled extension" $ do
      check s "{-# language CPP #-}" [("/extensions/Cpp", "true")]

    Spec.it s "works with a disabled extension" $ do
      check s "{-# language NoCPP #-}" [("/extensions/Cpp", "false")]

    Spec.it s "last one wins" $ do
      check s "{-# language CPP, NoCPP #-}" [("/extensions/Cpp", "false")]

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

    Spec.it s "works with a function" $ do
      -- Note that we call this a "function" because GHC does. Obviously it's
      -- just a value.
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

    Spec.it s "" $ do
      -- TODO
      pure ()

  -----------------------------------------------------------------------------

  Spec.describe s "items" $ do
    Spec.describe s "type signature" $ do
      Spec.it s "gets a function with type signature" $ do
        check
          s
          """
          foo :: Int -> Bool
          foo = undefined
          """
          [ ("/items/0/value/kind", "\"Function\""),
            ("/items/0/value/name", "\"foo\""),
            ("/items/0/value/parentKey", "null")
          ]

      Spec.it s "gets the signature text" $ do
        check
          s
          """
          foo :: Int -> Bool
          foo = undefined
          """
          [("/items/0/value/signature", "\"foo :: Int -> Bool\"")]

    Spec.describe s "data type" $ do
      Spec.it s "gets a data type" $ do
        check
          s
          """
          data T = A | B
          """
          [ ("/items/0/value/kind", "\"DataType\""),
            ("/items/0/value/name", "\"T\"")
          ]

      Spec.it s "gets data constructors as children" $ do
        check
          s
          """
          data T = A | B
          """
          [ ("/items/1/value/kind", "\"DataConstructor\""),
            ("/items/1/value/name", "\"A\""),
            ("/items/1/value/parentKey", "0"),
            ("/items/2/value/kind", "\"DataConstructor\""),
            ("/items/2/value/name", "\"B\""),
            ("/items/2/value/parentKey", "0")
          ]

    Spec.describe s "newtype" $ do
      Spec.it s "gets a newtype" $ do
        check
          s
          """
          newtype N = MkN Int
          """
          [ ("/items/0/value/kind", "\"Newtype\""),
            ("/items/0/value/name", "\"N\"")
          ]

      Spec.it s "gets the newtype constructor" $ do
        check
          s
          """
          newtype N = MkN Int
          """
          [ ("/items/1/value/kind", "\"DataConstructor\""),
            ("/items/1/value/name", "\"MkN\""),
            ("/items/1/value/parentKey", "0")
          ]

    Spec.describe s "type synonym" $ do
      Spec.it s "gets a type synonym" $ do
        check
          s
          """
          type S = Int
          """
          [ ("/items/0/value/kind", "\"TypeSynonym\""),
            ("/items/0/value/name", "\"S\"")
          ]

    Spec.describe s "class" $ do
      Spec.it s "gets a class" $ do
        check
          s
          """
          class C a where
            m :: a -> Int
          """
          [ ("/items/0/value/kind", "\"Class\""),
            ("/items/0/value/name", "\"C\"")
          ]

      Spec.it s "gets class methods as children" $ do
        check
          s
          """
          class C a where
            m :: a -> Int
          """
          [ ("/items/1/value/kind", "\"ClassMethod\""),
            ("/items/1/value/name", "\"m\""),
            ("/items/1/value/parentKey", "0")
          ]

    Spec.describe s "instance" $ do
      Spec.it s "gets a class instance" $ do
        check
          s
          """
          class C a
          instance C Int
          """
          [ ("/items/1/value/kind", "\"ClassInstance\"")
          ]

    Spec.describe s "record fields" $ do
      Spec.it s "gets record fields as children" $ do
        check
          s
          """
          data R = MkR { x :: Int, y :: Bool }
          """
          [ ("/items/0/value/kind", "\"DataType\""),
            ("/items/0/value/name", "\"R\""),
            ("/items/1/value/kind", "\"DataConstructor\""),
            ("/items/1/value/name", "\"MkR\""),
            ("/items/1/value/parentKey", "0"),
            ("/items/2/value/kind", "\"RecordField\""),
            ("/items/2/value/name", "\"x\""),
            ("/items/2/value/parentKey", "1"),
            ("/items/3/value/kind", "\"RecordField\""),
            ("/items/3/value/name", "\"y\""),
            ("/items/3/value/parentKey", "1")
          ]

    Spec.describe s "documentation on items" $ do
      Spec.it s "gets haddock on a function" $ do
        check
          s
          """
          -- | A useful function.
          foo :: Int -> Bool
          foo = undefined
          """
          [ ("/items/0/value/name", "\"foo\""),
            ("/items/0/value/documentation/type", "\"Paragraph\"")
          ]

      Spec.it s "gets haddock on a data type" $ do
        check
          s
          """
          -- | A data type.
          data T = A
          """
          [ ("/items/0/value/name", "\"T\""),
            ("/items/0/value/documentation/type", "\"Paragraph\"")
          ]

    Spec.describe s "standalone kind signature" $ do
      Spec.it s "gets a standalone kind signature" $ do
        check
          s
          """
          {-# LANGUAGE StandaloneKindSignatures #-}
          type T :: *
          type T = Int
          """
          [ ("/items/0/value/kind", "\"StandaloneKindSig\""),
            ("/items/0/value/name", "\"T\"")
          ]

    Spec.describe s "fixity" $ do
      Spec.it s "gets a fixity declaration" $ do
        check
          s
          """
          infixl 6 +!
          (+!) = undefined
          """
          [ ("/items/0/value/kind", "\"FixitySignature\"")
          ]

    Spec.describe s "deriving" $ do
      Spec.it s "gets derived instances" $ do
        check
          s
          """
          data T = A deriving (Eq, Ord)
          """
          [ ("/items/0/value/kind", "\"DataType\""),
            ("/items/0/value/name", "\"T\""),
            ("/items/1/value/kind", "\"DataConstructor\""),
            ("/items/2/value/kind", "\"DerivedInstance\""),
            ("/items/2/value/parentKey", "0"),
            ("/items/3/value/kind", "\"DerivedInstance\""),
            ("/items/3/value/parentKey", "0")
          ]

    Spec.describe s "GADT" $ do
      Spec.it s "gets a GADT constructor" $ do
        check
          s
          """
          {-# LANGUAGE GADTs #-}
          data T where
            MkT :: Int -> T
          """
          [ ("/items/0/value/kind", "\"DataType\""),
            ("/items/0/value/name", "\"T\""),
            ("/items/1/value/kind", "\"GADTConstructor\""),
            ("/items/1/value/name", "\"MkT\""),
            ("/items/1/value/parentKey", "0")
          ]

    Spec.describe s "pattern synonym" $ do
      Spec.it s "gets a pattern synonym" $ do
        check
          s
          """
          {-# LANGUAGE PatternSynonyms #-}
          pattern P = 42
          """
          [ ("/items/0/value/kind", "\"PatternSynonym\""),
            ("/items/0/value/name", "\"P\"")
          ]

    Spec.describe s "foreign import" $ do
      Spec.it s "gets a foreign import" $ do
        check
          s
          """
          {-# LANGUAGE ForeignFunctionInterface #-}
          foreign import ccall "foo" c_foo :: Int -> Int
          """
          [ ("/items/0/value/kind", "\"ForeignImport\"")
          ]

    Spec.describe s "default declaration" $ do
      Spec.it s "gets a default declaration" $ do
        check
          s
          """
          default (Int, Double)
          """
          [("/items/0/value/kind", "\"Default\"")]

    Spec.describe s "type family" $ do
      Spec.it s "gets a type family" $ do
        check
          s
          """
          {-# LANGUAGE TypeFamilies #-}
          type family F a
          """
          [ ("/items/0/value/kind", "\"OpenTypeFamily\""),
            ("/items/0/value/name", "\"F\"")
          ]

      Spec.it s "gets a data family" $ do
        check
          s
          """
          {-# LANGUAGE TypeFamilies #-}
          data family D a
          """
          [ ("/items/0/value/kind", "\"DataFamily\""),
            ("/items/0/value/name", "\"D\"")
          ]

      Spec.it s "gets a closed type family" $ do
        check
          s
          """
          {-# LANGUAGE TypeFamilies #-}
          type family F a where
            F Int = Bool
          """
          [ ("/items/0/value/kind", "\"ClosedTypeFamily\""),
            ("/items/0/value/name", "\"F\"")
          ]

    Spec.describe s "standalone deriving" $ do
      Spec.it s "gets a standalone deriving declaration" $ do
        check
          s
          """
          {-# LANGUAGE StandaloneDeriving #-}
          data T = A
          deriving instance Eq T
          """
          [ ("/items/2/value/kind", "\"StandaloneDeriving\"")
          ]

    Spec.describe s "class with associated type" $ do
      Spec.it s "gets associated type families as children" $ do
        check
          s
          """
          {-# LANGUAGE TypeFamilies #-}
          class C a where
            type F a
          """
          [ ("/items/0/value/kind", "\"Class\""),
            ("/items/0/value/name", "\"C\""),
            ("/items/1/value/kind", "\"OpenTypeFamily\""),
            ("/items/1/value/name", "\"F\""),
            ("/items/1/value/parentKey", "0")
          ]

    Spec.describe s "merging" $ do
      Spec.it s "merges a type signature with its binding" $ do
        check
          s
          """
          foo :: Int
          foo = 42
          """
          [ ("/items/0/value/name", "\"foo\""),
            ("/items/0/value/signature", "\"foo :: Int\""),
            ("/items/1/value/name", "")
          ]

    Spec.describe s "location" $ do
      Spec.it s "gets the line and column of an item" $ do
        check
          s
          """
          foo :: Int
          foo = 0
          """
          [ ("/items/0/location/line", "1"),
            ("/items/0/location/column", "1")
          ]

      Spec.it s "gets the location of a second item" $ do
        check
          s
          """
          foo :: Int
          foo = 0
          bar :: Bool
          bar = True
          """
          [ ("/items/0/location/line", "1"),
            ("/items/1/location/line", "3")
          ]

    Spec.describe s "multiple items" $ do
      Spec.it s "gets multiple different declaration types" $ do
        check
          s
          """
          module M where
          data T = A
          type S = T
          foo :: S -> Int
          foo = undefined
          """
          [ ("/name/value", "\"M\""),
            ("/items/0/value/kind", "\"DataType\""),
            ("/items/0/value/name", "\"T\""),
            ("/items/1/value/kind", "\"DataConstructor\""),
            ("/items/1/value/name", "\"A\""),
            ("/items/2/value/kind", "\"TypeSynonym\""),
            ("/items/2/value/name", "\"S\""),
            ("/items/3/value/kind", "\"Function\""),
            ("/items/3/value/name", "\"foo\"")
          ]

check :: (Stack.HasCallStack, Monad m) => Spec.Spec m n -> String -> [(String, String)] -> m ()
check s input assertions = do
  parsed <- either (Spec.assertFailure s) pure $ Parse.parse input
  module_ <- either (Spec.assertFailure s) pure $ FromGhc.fromGhc parsed
  let json = ToJson.toJson module_
  Monad.forM_ assertions $ \(p, j) -> do
    pointer <- maybe (Spec.assertFailure s "invalid pointer") pure $ Parsec.parseString Pointer.decode p
    maybeJson <-
      if null j
        then pure Nothing
        else maybe (Spec.assertFailure s "invalid json") (pure . Just) $ Parsec.parseString Json.decode j
    Spec.assertEq s (Pointer.evaluate pointer json) maybeJson
