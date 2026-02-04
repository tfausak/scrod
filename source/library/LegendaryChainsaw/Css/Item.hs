{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module LegendaryChainsaw.Css.Item where

import qualified Data.ByteString.Builder as Builder
import qualified Data.Text as Text
import qualified LegendaryChainsaw.Css.AtRule as AtRule
import qualified LegendaryChainsaw.Css.Block as Block
import qualified LegendaryChainsaw.Css.BlockContent as BlockContent
import qualified LegendaryChainsaw.Css.Declaration as Declaration
import qualified LegendaryChainsaw.Css.Name as Name
import qualified LegendaryChainsaw.Css.Rule as Rule
import qualified LegendaryChainsaw.Css.Selector as Selector
import qualified LegendaryChainsaw.Extra.Builder as Builder
import qualified LegendaryChainsaw.Extra.Parsec as Parsec
import qualified LegendaryChainsaw.Spec as Spec
import qualified Text.Parsec as Parsec

-- | CSS Item (top-level or nested)
-- Either a style rule or an at-rule
data Item
  = StyleRule Rule.Rule
  | ItemAtRule (AtRule.AtRule Item)
  deriving (Eq, Ord, Show)

decode :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m Item
decode =
  Parsec.choice
    [ ItemAtRule <$> AtRule.decode decode,
      StyleRule <$> Rule.decode
    ]

encode :: Item -> Builder.Builder
encode item = case item of
  StyleRule rule -> Rule.encode rule
  ItemAtRule atRule -> AtRule.encode encode atRule

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = do
  Spec.named s 'decode $ do
    Spec.it s "succeeds with style rule" $ do
      (Spec.assertEq s (Parsec.parseString decode "div { color: red }") . Just . StyleRule)
        ( Rule.MkRule
            [Selector.MkSelector $ Text.pack "div"]
            [Declaration.MkDeclaration (Name.MkName $ Text.pack "color") (Text.pack "red") False]
        )

    Spec.it s "succeeds with at-rule" $ do
      (Spec.assertEq s (Parsec.parseString decode "@import url(\"foo.css\");") . Just . ItemAtRule)
        (AtRule.MkAtRule (Name.MkName $ Text.pack "import") (Text.pack "url(\"foo.css\")") Nothing)

    Spec.it s "succeeds with media rule" $ do
      (Spec.assertEq s (Parsec.parseString decode "@media screen { }") . Just . ItemAtRule)
        (AtRule.MkAtRule (Name.MkName $ Text.pack "media") (Text.pack "screen") (Just $ Block.MkBlock []))

    Spec.it s "succeeds with media rule containing style rule" $ do
      (Spec.assertEq s (Parsec.parseString decode "@media screen { div { color: red } }") . Just . ItemAtRule)
        ( AtRule.MkAtRule
            (Name.MkName $ Text.pack "media")
            (Text.pack "screen")
            ( Just $
                Block.MkBlock
                  [ BlockContent.ContentItem . StyleRule $
                      Rule.MkRule
                        [Selector.MkSelector $ Text.pack "div"]
                        [Declaration.MkDeclaration (Name.MkName $ Text.pack "color") (Text.pack "red") False]
                  ]
            )
        )

    Spec.it s "succeeds with nested media rules" $ do
      (Spec.assertEq s (Parsec.parseString decode "@media screen { @media print { } }") . Just . ItemAtRule)
        ( AtRule.MkAtRule
            (Name.MkName $ Text.pack "media")
            (Text.pack "screen")
            ( Just $
                Block.MkBlock
                  [ BlockContent.ContentItem . ItemAtRule $
                      AtRule.MkAtRule (Name.MkName $ Text.pack "media") (Text.pack "print") (Just $ Block.MkBlock [])
                  ]
            )
        )

    Spec.it s "succeeds with font-face" $ do
      (Spec.assertEq s (Parsec.parseString decode "@font-face { font-family: \"Open Sans\" }") . Just . ItemAtRule)
        ( AtRule.MkAtRule
            (Name.MkName $ Text.pack "font-face")
            (Text.pack "")
            ( Just $
                Block.MkBlock
                  [ BlockContent.ContentDeclaration $
                      Declaration.MkDeclaration (Name.MkName $ Text.pack "font-family") (Text.pack "\"Open Sans\"") False
                  ]
            )
        )

  Spec.named s 'encode $ do
    Spec.it s "encodes style rule" $ do
      Spec.assertEq
        s
        ( (Builder.toString . encode) . StyleRule $
            Rule.MkRule
              [Selector.MkSelector $ Text.pack "div"]
              [Declaration.MkDeclaration (Name.MkName $ Text.pack "color") (Text.pack "red") False]
        )
        "div { color: red; }"

    Spec.it s "encodes at-rule" $ do
      Spec.assertEq
        s
        ( (Builder.toString . encode) . ItemAtRule $
            AtRule.MkAtRule (Name.MkName $ Text.pack "import") (Text.pack "url(\"foo.css\")") Nothing
        )
        "@import url(\"foo.css\");"

    Spec.it s "encodes media rule" $ do
      Spec.assertEq
        s
        ( (Builder.toString . encode) . ItemAtRule $
            AtRule.MkAtRule (Name.MkName $ Text.pack "media") (Text.pack "screen") (Just $ Block.MkBlock [])
        )
        "@media screen { }"

    Spec.it s "encodes media rule with content" $ do
      Spec.assertEq
        s
        ( (Builder.toString . encode) . ItemAtRule $
            AtRule.MkAtRule
              (Name.MkName $ Text.pack "media")
              (Text.pack "screen")
              ( Just $
                  Block.MkBlock
                    [ BlockContent.ContentItem . StyleRule $
                        Rule.MkRule
                          [Selector.MkSelector $ Text.pack "div"]
                          [Declaration.MkDeclaration (Name.MkName $ Text.pack "color") (Text.pack "red") False]
                    ]
              )
        )
        "@media screen { div { color: red; }; }"
