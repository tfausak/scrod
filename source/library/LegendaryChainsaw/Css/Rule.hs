{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module LegendaryChainsaw.Css.Rule where

import qualified Data.ByteString.Builder as Builder
import qualified Data.List as List
import qualified Data.Text as Text
import qualified LegendaryChainsaw.Css.Declaration as Declaration
import qualified LegendaryChainsaw.Css.Name as Name
import qualified LegendaryChainsaw.Css.Selector as Selector
import qualified LegendaryChainsaw.Extra.Builder as Builder
import qualified LegendaryChainsaw.Extra.Parsec as Parsec
import qualified LegendaryChainsaw.Spec as Spec
import qualified Text.Parsec as Parsec

-- | CSS Style Rule
-- selectors { declarations }
data Rule = MkRule
  { selectors :: [Selector.Selector],
    declarations :: [Declaration.Declaration]
  }
  deriving (Eq, Ord, Show)

decode :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m Rule
decode = do
  _ <- Parsec.many Parsec.blank
  sels <- decodeSelectors
  _ <- Parsec.many Parsec.blank
  _ <- Parsec.char '{'
  _ <- Parsec.many Parsec.blank
  decls <- Parsec.many $ Declaration.decode <* Parsec.many Parsec.blank
  _ <- Parsec.char '}'
  pure $ MkRule sels decls

-- | Decode comma-separated selectors
decodeSelectors :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m [Selector.Selector]
decodeSelectors = do
  first <- Selector.decode
  rest <- Parsec.many $ Parsec.char ',' *> Parsec.many Parsec.blank *> Selector.decode
  pure $ first : rest

encode :: Rule -> Builder.Builder
encode rule =
  mconcat (List.intersperse (Builder.stringUtf8 ", ") $ fmap Selector.encode (selectors rule))
    <> Builder.stringUtf8 " {"
    <> foldMap (\d -> Builder.charUtf8 ' ' <> Declaration.encode d <> Builder.charUtf8 ';') (declarations rule)
    <> Builder.stringUtf8 " }"

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = do
  Spec.named s 'decode $ do
    Spec.it s "succeeds with simple rule" $ do
      Spec.assertEq s (Parsec.parseString decode "div { color: red }") . Just $
        MkRule
          [Selector.MkSelector $ Text.pack "div"]
          [Declaration.MkDeclaration (Name.MkName $ Text.pack "color") (Text.pack "red") False]

    Spec.it s "succeeds with empty declarations" $ do
      Spec.assertEq s (Parsec.parseString decode "div { }") . Just $
        MkRule [Selector.MkSelector $ Text.pack "div"] []

    Spec.it s "succeeds with multiple selectors" $ do
      Spec.assertEq s (Parsec.parseString decode "div, p { color: red }") . Just $
        MkRule
          [Selector.MkSelector $ Text.pack "div", Selector.MkSelector $ Text.pack "p"]
          [Declaration.MkDeclaration (Name.MkName $ Text.pack "color") (Text.pack "red") False]

    Spec.it s "succeeds with multiple declarations" $ do
      Spec.assertEq s (Parsec.parseString decode "div { color: red; font-size: 12px }") . Just $
        MkRule
          [Selector.MkSelector $ Text.pack "div"]
          [ Declaration.MkDeclaration (Name.MkName $ Text.pack "color") (Text.pack "red") False,
            Declaration.MkDeclaration (Name.MkName $ Text.pack "font-size") (Text.pack "12px") False
          ]

    Spec.it s "succeeds with class selector" $ do
      Spec.assertEq s (Parsec.parseString decode ".foo { color: red }") . Just $
        MkRule
          [Selector.MkSelector $ Text.pack ".foo"]
          [Declaration.MkDeclaration (Name.MkName $ Text.pack "color") (Text.pack "red") False]

    Spec.it s "succeeds with complex selectors" $ do
      Spec.assertEq s (Parsec.parseString decode "div.foo > p:first-child, ul li { color: red }") . Just $
        MkRule
          [Selector.MkSelector $ Text.pack "div.foo > p:first-child", Selector.MkSelector $ Text.pack "ul li"]
          [Declaration.MkDeclaration (Name.MkName $ Text.pack "color") (Text.pack "red") False]

    Spec.it s "succeeds with leading whitespace" $ do
      Spec.assertEq s (Parsec.parseString decode "  div { color: red }") . Just $
        MkRule
          [Selector.MkSelector $ Text.pack "div"]
          [Declaration.MkDeclaration (Name.MkName $ Text.pack "color") (Text.pack "red") False]

    Spec.it s "fails without opening brace" $ do
      Spec.assertEq s (Parsec.parseString decode "div color: red }") Nothing

    Spec.it s "fails without closing brace" $ do
      Spec.assertEq s (Parsec.parseString decode "div { color: red") Nothing

  Spec.named s 'encode $ do
    Spec.it s "encodes simple rule" $ do
      Spec.assertEq
        s
        ( Builder.toString . encode $
            MkRule
              [Selector.MkSelector $ Text.pack "div"]
              [Declaration.MkDeclaration (Name.MkName $ Text.pack "color") (Text.pack "red") False]
        )
        "div { color: red; }"

    Spec.it s "encodes empty rule" $ do
      Spec.assertEq
        s
        (Builder.toString . encode $ MkRule [Selector.MkSelector $ Text.pack "div"] [])
        "div { }"

    Spec.it s "encodes multiple selectors" $ do
      Spec.assertEq
        s
        ( Builder.toString . encode $
            MkRule
              [Selector.MkSelector $ Text.pack "div", Selector.MkSelector $ Text.pack "p"]
              [Declaration.MkDeclaration (Name.MkName $ Text.pack "color") (Text.pack "red") False]
        )
        "div, p { color: red; }"

    Spec.it s "encodes multiple declarations" $ do
      Spec.assertEq
        s
        ( Builder.toString . encode $
            MkRule
              [Selector.MkSelector $ Text.pack "div"]
              [ Declaration.MkDeclaration (Name.MkName $ Text.pack "color") (Text.pack "red") False,
                Declaration.MkDeclaration (Name.MkName $ Text.pack "font-size") (Text.pack "12px") False
              ]
        )
        "div { color: red; font-size: 12px; }"
