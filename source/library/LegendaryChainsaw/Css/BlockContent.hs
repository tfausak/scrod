{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module LegendaryChainsaw.Css.BlockContent where

import qualified Data.ByteString.Builder as Builder
import qualified Data.Text as Text
import qualified LegendaryChainsaw.Css.Declaration as Declaration
import qualified LegendaryChainsaw.Css.Name as Name
import qualified LegendaryChainsaw.Extra.Builder as Builder
import qualified LegendaryChainsaw.Extra.Parsec as Parsec
import qualified LegendaryChainsaw.Spec as Spec
import qualified Text.Parsec as Parsec

-- | CSS Block Content
-- Parameterized by item type to avoid circular dependencies.
-- Item.hs uses BlockContent Item.
data BlockContent a
  = ContentDeclaration Declaration.Declaration
  | ContentItem a
  deriving (Eq, Ord, Show)

-- | Decode block content, parameterized by item decoder
decode :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m a -> Parsec.ParsecT s u m (BlockContent a)
decode itemParser =
  Parsec.choice
    [ Parsec.try $ ContentItem <$> itemParser,
      ContentDeclaration <$> Declaration.decode
    ]

-- | Encode block content, parameterized by item encoder
encode :: (a -> Builder.Builder) -> BlockContent a -> Builder.Builder
encode encodeItem bc = case bc of
  ContentDeclaration decl -> Declaration.encode decl
  ContentItem item -> encodeItem item

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = do
  Spec.named s 'decode $ do
    -- Use a simple item parser for testing (matches "@test")
    let itemP :: (Parsec.Stream t m Char) => Parsec.ParsecT t u m String
        itemP = "test" <$ Parsec.string "@test"

    Spec.it s "succeeds with declaration" $ do
      Spec.assertEq s (Parsec.parseString (decode itemP) "color: red") . Just $
        ContentDeclaration (Declaration.MkDeclaration (Name.MkName $ Text.pack "color") (Text.pack "red") False)

    Spec.it s "succeeds with item" $ do
      Spec.assertEq s (Parsec.parseString (decode itemP) "@test") . Just $
        ContentItem "test"

    Spec.it s "prefers item over declaration" $ do
      -- If it looks like an item, parse as item
      Spec.assertEq s (Parsec.parseString (decode itemP) "@test") . Just $
        ContentItem "test"

  Spec.named s 'encode $ do
    let encodeItem :: String -> Builder.Builder
        encodeItem _ = Builder.stringUtf8 "@test"

    Spec.it s "encodes declaration" $ do
      Spec.assertEq
        s
        ( Builder.toString . encode encodeItem $
            ContentDeclaration (Declaration.MkDeclaration (Name.MkName $ Text.pack "color") (Text.pack "red") False)
        )
        "color: red"

    Spec.it s "encodes item" $ do
      Spec.assertEq s (Builder.toString . encode encodeItem $ ContentItem "test") "@test"
