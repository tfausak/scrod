{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module LegendaryChainsaw.Css.AtRule where

import qualified Data.ByteString.Builder as Builder
import qualified Data.Text as Text
import qualified LegendaryChainsaw.Css.Block as Block
import qualified LegendaryChainsaw.Css.Name as Name
import qualified LegendaryChainsaw.Extra.Builder as Builder
import qualified LegendaryChainsaw.Extra.Parsec as Parsec
import qualified LegendaryChainsaw.Spec as Spec
import qualified Text.Parsec as Parsec

-- | CSS At-Rule
-- @name prelude { block } or @name prelude;
-- Parameterized by item type for nested rules.
data AtRule a = MkAtRule
  { name :: Name.Name,
    prelude :: Text.Text,
    block :: Maybe (Block.Block a)
  }
  deriving (Eq, Ord, Show)

-- | Decode an at-rule, parameterized by item decoder (for nested rules)
decode :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m a -> Parsec.ParsecT s u m (AtRule a)
decode itemParser = do
  _ <- Parsec.char '@'
  n <- Name.decode
  _ <- Parsec.many Parsec.blank
  p <- decodePrelude
  _ <- Parsec.many Parsec.blank
  b <- Parsec.optionMaybe $ Block.decode itemParser
  case b of
    Nothing -> Parsec.optional $ Parsec.char ';'
    Just _ -> pure ()
  pure $ MkAtRule n (Text.strip p) b

-- | Decode prelude - everything up to { or ;
decodePrelude :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m Text.Text
decodePrelude = Text.pack <$> Parsec.many (Parsec.satisfy $ \c -> c /= '{' && c /= ';')

-- | Encode an at-rule, parameterized by item encoder
encode :: (a -> Builder.Builder) -> AtRule a -> Builder.Builder
encode encodeItem ar =
  Builder.charUtf8 '@'
    <> Name.encode (name ar)
    <> (if Text.null (prelude ar) then mempty else Builder.charUtf8 ' ' <> Builder.stringUtf8 (Text.unpack $ prelude ar))
    <> case block ar of
      Nothing -> Builder.charUtf8 ';'
      Just b -> Builder.charUtf8 ' ' <> Block.encode encodeItem b

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = do
  Spec.named s 'decode $ do
    -- Use a simple item parser for testing
    let itemP :: (Parsec.Stream t m Char) => Parsec.ParsecT t u m String
        itemP = "nested" <$ Parsec.string "@nested { }"

    Spec.it s "succeeds with statement at-rule" $ do
      Spec.assertEq s (Parsec.parseString (decode itemP) "@import url(\"foo.css\");") . Just $
        MkAtRule (Name.MkName $ Text.pack "import") (Text.pack "url(\"foo.css\")") Nothing

    Spec.it s "succeeds with statement at-rule without semicolon" $ do
      Spec.assertEq s (Parsec.parseString (decode itemP) "@charset \"UTF-8\"") . Just $
        MkAtRule (Name.MkName $ Text.pack "charset") (Text.pack "\"UTF-8\"") Nothing

    Spec.it s "succeeds with empty block at-rule" $ do
      Spec.assertEq s (Parsec.parseString (decode itemP) "@media screen { }") . Just $
        MkAtRule (Name.MkName $ Text.pack "media") (Text.pack "screen") (Just $ Block.MkBlock [])

    Spec.it s "succeeds with no prelude" $ do
      Spec.assertEq s (Parsec.parseString (decode itemP) "@font-face { }") . Just $
        MkAtRule (Name.MkName $ Text.pack "font-face") (Text.pack "") (Just $ Block.MkBlock [])

    Spec.it s "succeeds with complex prelude" $ do
      Spec.assertEq s (Parsec.parseString (decode itemP) "@media screen and (min-width: 768px) { }") . Just $
        MkAtRule (Name.MkName $ Text.pack "media") (Text.pack "screen and (min-width: 768px)") (Just $ Block.MkBlock [])

    Spec.it s "succeeds with keyframes" $ do
      Spec.assertEq s (Parsec.parseString (decode itemP) "@keyframes slide { }") . Just $
        MkAtRule (Name.MkName $ Text.pack "keyframes") (Text.pack "slide") (Just $ Block.MkBlock [])

    Spec.it s "fails without @ symbol" $ do
      Spec.assertEq s (Parsec.parseString (decode itemP) "media screen { }") Nothing

  Spec.named s 'encode $ do
    let encodeItem :: String -> Builder.Builder
        encodeItem _ = Builder.stringUtf8 "@nested { }"

    Spec.it s "encodes statement at-rule" $ do
      Spec.assertEq
        s
        (Builder.toString . encode encodeItem $ MkAtRule (Name.MkName $ Text.pack "import") (Text.pack "url(\"foo.css\")") Nothing)
        "@import url(\"foo.css\");"

    Spec.it s "encodes block at-rule with prelude" $ do
      Spec.assertEq
        s
        (Builder.toString . encode encodeItem $ MkAtRule (Name.MkName $ Text.pack "media") (Text.pack "screen") (Just $ Block.MkBlock []))
        "@media screen { }"

    Spec.it s "encodes block at-rule without prelude" $ do
      Spec.assertEq
        s
        (Builder.toString . encode encodeItem $ MkAtRule (Name.MkName $ Text.pack "font-face") (Text.pack "") (Just $ Block.MkBlock []))
        "@font-face { }"
