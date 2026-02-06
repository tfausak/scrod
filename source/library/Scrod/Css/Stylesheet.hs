{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Scrod.Css.Stylesheet where

import qualified Data.ByteString.Builder as Builder
import qualified Data.Text as Text
import qualified Scrod.Css.AtRule as AtRule
import qualified Scrod.Css.Block as Block
import qualified Scrod.Css.BlockContent as BlockContent
import qualified Scrod.Css.Declaration as Declaration
import qualified Scrod.Css.Item as Item
import qualified Scrod.Css.Name as Name
import qualified Scrod.Css.Rule as Rule
import qualified Scrod.Css.Selector as Selector
import qualified Scrod.Extra.Builder as Builder
import qualified Scrod.Extra.Parsec as Parsec
import qualified Scrod.Spec as Spec
import qualified Text.Parsec as Parsec

-- | CSS Stylesheet
-- A list of top-level items (style rules and at-rules)
newtype Stylesheet = MkStylesheet
  { items :: [Item.Item]
  }
  deriving (Eq, Ord, Show)

decode :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m Stylesheet
decode = do
  _ <- Parsec.many Parsec.blank
  is <- Parsec.many $ Item.decode <* Parsec.many Parsec.blank
  Parsec.eof
  pure $ MkStylesheet is

encode :: Stylesheet -> Builder.Builder
encode (MkStylesheet is) =
  mconcat $ fmap (\i -> Item.encode i <> Builder.charUtf8 '\n') is

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = do
  Spec.named s 'decode $ do
    Spec.it s "succeeds with empty stylesheet" $ do
      Spec.assertEq s (Parsec.parseString decode "") . Just $
        MkStylesheet []

    Spec.it s "succeeds with whitespace only" $ do
      Spec.assertEq s (Parsec.parseString decode "   ") . Just $
        MkStylesheet []

    Spec.it s "succeeds with single style rule" $ do
      Spec.assertEq s (Parsec.parseString decode "div { color: red }") . Just $
        MkStylesheet
          [ Item.StyleRule $
              Rule.MkRule
                [Selector.MkSelector $ Text.pack "div"]
                [Declaration.MkDeclaration (Name.MkName $ Text.pack "color") (Text.pack "red") False]
          ]

    Spec.it s "succeeds with multiple style rules" $ do
      Spec.assertEq s (Parsec.parseString decode "div { color: red } p { font-size: 12px }") . Just $
        MkStylesheet
          [ Item.StyleRule $
              Rule.MkRule
                [Selector.MkSelector $ Text.pack "div"]
                [Declaration.MkDeclaration (Name.MkName $ Text.pack "color") (Text.pack "red") False],
            Item.StyleRule $
              Rule.MkRule
                [Selector.MkSelector $ Text.pack "p"]
                [Declaration.MkDeclaration (Name.MkName $ Text.pack "font-size") (Text.pack "12px") False]
          ]

    Spec.it s "succeeds with import and style rule" $ do
      Spec.assertEq s (Parsec.parseString decode "@import url(\"reset.css\"); div { color: red }") . Just $
        MkStylesheet
          [ Item.ItemAtRule $
              AtRule.MkAtRule (Name.MkName $ Text.pack "import") (Text.pack "url(\"reset.css\")") Nothing,
            Item.StyleRule $
              Rule.MkRule
                [Selector.MkSelector $ Text.pack "div"]
                [Declaration.MkDeclaration (Name.MkName $ Text.pack "color") (Text.pack "red") False]
          ]

    Spec.it s "succeeds with media query" $ do
      Spec.assertEq s (Parsec.parseString decode "@media screen { div { color: red } }") . Just $
        MkStylesheet
          [ Item.ItemAtRule $
              AtRule.MkAtRule
                (Name.MkName $ Text.pack "media")
                (Text.pack "screen")
                ( Just $
                    Block.MkBlock
                      [ BlockContent.ContentItem . Item.StyleRule $
                          Rule.MkRule
                            [Selector.MkSelector $ Text.pack "div"]
                            [Declaration.MkDeclaration (Name.MkName $ Text.pack "color") (Text.pack "red") False]
                      ]
                )
          ]

    Spec.it s "succeeds with complex stylesheet" $ do
      let input = "@charset \"UTF-8\"; @import url(\"base.css\"); body { margin: 0 } @media print { body { color: black } }"
      Spec.assertEq s (Parsec.parseString decode input) . Just $
        MkStylesheet
          [ Item.ItemAtRule $
              AtRule.MkAtRule (Name.MkName $ Text.pack "charset") (Text.pack "\"UTF-8\"") Nothing,
            Item.ItemAtRule $
              AtRule.MkAtRule (Name.MkName $ Text.pack "import") (Text.pack "url(\"base.css\")") Nothing,
            Item.StyleRule $
              Rule.MkRule
                [Selector.MkSelector $ Text.pack "body"]
                [Declaration.MkDeclaration (Name.MkName $ Text.pack "margin") (Text.pack "0") False],
            Item.ItemAtRule $
              AtRule.MkAtRule
                (Name.MkName $ Text.pack "media")
                (Text.pack "print")
                ( Just $
                    Block.MkBlock
                      [ BlockContent.ContentItem . Item.StyleRule $
                          Rule.MkRule
                            [Selector.MkSelector $ Text.pack "body"]
                            [Declaration.MkDeclaration (Name.MkName $ Text.pack "color") (Text.pack "black") False]
                      ]
                )
          ]

  Spec.named s 'encode $ do
    Spec.it s "encodes empty stylesheet" $ do
      Spec.assertEq s (Builder.toString . encode $ MkStylesheet []) ""

    Spec.it s "encodes single rule" $ do
      Spec.assertEq
        s
        ( Builder.toString . encode $
            MkStylesheet
              [ Item.StyleRule $
                  Rule.MkRule
                    [Selector.MkSelector $ Text.pack "div"]
                    [Declaration.MkDeclaration (Name.MkName $ Text.pack "color") (Text.pack "red") False]
              ]
        )
        "div { color: red; }\n"

    Spec.it s "encodes multiple rules" $ do
      Spec.assertEq
        s
        ( Builder.toString . encode $
            MkStylesheet
              [ Item.StyleRule $
                  Rule.MkRule
                    [Selector.MkSelector $ Text.pack "div"]
                    [Declaration.MkDeclaration (Name.MkName $ Text.pack "color") (Text.pack "red") False],
                Item.StyleRule $
                  Rule.MkRule
                    [Selector.MkSelector $ Text.pack "p"]
                    [Declaration.MkDeclaration (Name.MkName $ Text.pack "font-size") (Text.pack "12px") False]
              ]
        )
        "div { color: red; }\np { font-size: 12px; }\n"

    Spec.it s "encodes import and rule" $ do
      Spec.assertEq
        s
        ( Builder.toString . encode $
            MkStylesheet
              [ Item.ItemAtRule $
                  AtRule.MkAtRule (Name.MkName $ Text.pack "import") (Text.pack "url(\"reset.css\")") Nothing,
                Item.StyleRule $
                  Rule.MkRule
                    [Selector.MkSelector $ Text.pack "div"]
                    [Declaration.MkDeclaration (Name.MkName $ Text.pack "color") (Text.pack "red") False]
              ]
        )
        "@import url(\"reset.css\");\ndiv { color: red; }\n"
