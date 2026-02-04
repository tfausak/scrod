{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module LegendaryChainsaw.Css.Block where

import qualified Data.ByteString.Builder as Builder
import qualified Data.Text as Text
import qualified LegendaryChainsaw.Css.BlockContent as BlockContent
import qualified LegendaryChainsaw.Css.Declaration as Declaration
import qualified LegendaryChainsaw.Css.Name as Name
import qualified LegendaryChainsaw.Extra.Builder as Builder
import qualified LegendaryChainsaw.Extra.Parsec as Parsec
import qualified LegendaryChainsaw.Spec as Spec
import qualified Text.Parsec as Parsec

-- | CSS Block (contents of { ... })
-- Parameterized by item type to avoid circular dependencies.
newtype Block a = MkBlock
  { contents :: [BlockContent.BlockContent a]
  }
  deriving (Eq, Ord, Show)

-- | Decode a block { ... }, parameterized by item decoder
decode :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m a -> Parsec.ParsecT s u m (Block a)
decode itemParser = do
  _ <- Parsec.char '{'
  _ <- Parsec.many Parsec.blank
  cs <- Parsec.many $ BlockContent.decode itemParser <* Parsec.many Parsec.blank
  _ <- Parsec.char '}'
  pure $ MkBlock cs

-- | Encode a block, parameterized by item encoder
encode :: (a -> Builder.Builder) -> Block a -> Builder.Builder
encode encodeItem (MkBlock cs) =
  Builder.charUtf8 '{'
    <> foldMap (\c -> Builder.charUtf8 ' ' <> BlockContent.encode encodeItem c <> Builder.charUtf8 ';') cs
    <> Builder.stringUtf8 " }"

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = do
  Spec.named s 'decode $ do
    -- Use a simple item parser for testing
    let itemP :: (Parsec.Stream t m Char) => Parsec.ParsecT t u m String
        itemP = "test" <$ Parsec.string "@test { }"

    Spec.it s "succeeds with empty block" $ do
      Spec.assertEq s (Parsec.parseString (decode itemP) "{}") . Just $
        MkBlock []

    Spec.it s "succeeds with single declaration" $ do
      Spec.assertEq s (Parsec.parseString (decode itemP) "{ color: red }") . Just $
        MkBlock [BlockContent.ContentDeclaration $ Declaration.MkDeclaration (Name.MkName $ Text.pack "color") (Text.pack "red") False]

    Spec.it s "succeeds with multiple declarations" $ do
      Spec.assertEq s (Parsec.parseString (decode itemP) "{ color: red; font-size: 12px }") . Just $
        MkBlock
          [ BlockContent.ContentDeclaration $ Declaration.MkDeclaration (Name.MkName $ Text.pack "color") (Text.pack "red") False,
            BlockContent.ContentDeclaration $ Declaration.MkDeclaration (Name.MkName $ Text.pack "font-size") (Text.pack "12px") False
          ]

    Spec.it s "succeeds with nested item" $ do
      Spec.assertEq s (Parsec.parseString (decode itemP) "{ @test { } }") . Just $
        MkBlock [BlockContent.ContentItem "test"]

    Spec.it s "succeeds with whitespace" $ do
      Spec.assertEq s (Parsec.parseString (decode itemP) "{  }") . Just $
        MkBlock []

    Spec.it s "fails without closing brace" $ do
      Spec.assertEq s (Parsec.parseString (decode itemP) "{ color: red") Nothing

  Spec.named s 'encode $ do
    let encodeItem :: String -> Builder.Builder
        encodeItem _ = Builder.stringUtf8 "@test { }"

    Spec.it s "encodes empty block" $ do
      Spec.assertEq s (Builder.toString . encode encodeItem $ MkBlock []) "{ }"

    Spec.it s "encodes single declaration" $ do
      Spec.assertEq
        s
        ( Builder.toString . encode encodeItem $
            MkBlock [BlockContent.ContentDeclaration $ Declaration.MkDeclaration (Name.MkName $ Text.pack "color") (Text.pack "red") False]
        )
        "{ color: red; }"

    Spec.it s "encodes multiple declarations" $ do
      Spec.assertEq
        s
        ( Builder.toString . encode encodeItem $
            MkBlock
              [ BlockContent.ContentDeclaration $ Declaration.MkDeclaration (Name.MkName $ Text.pack "color") (Text.pack "red") False,
                BlockContent.ContentDeclaration $ Declaration.MkDeclaration (Name.MkName $ Text.pack "font-size") (Text.pack "12px") False
              ]
        )
        "{ color: red; font-size: 12px; }"
