{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module LegendaryChainsaw.Xml.Instruction where

import qualified Data.ByteString.Builder as Builder
import qualified Data.Text as Text
import qualified LegendaryChainsaw.Extra.Builder as Builder
import qualified LegendaryChainsaw.Extra.Parsec as Parsec
import qualified LegendaryChainsaw.Extra.Semigroup as Semigroup
import qualified LegendaryChainsaw.Spec as Spec
import qualified LegendaryChainsaw.Xml.Name as Name
import qualified Text.Parsec as Parsec

-- | XML Processing Instruction
-- <?name value?>
-- If value is empty, no space after the name
-- Otherwise there must be a space after the name
-- Cannot contain "?>"
data Instruction = MkInstruction
  { name :: Name.Name,
    value :: Text.Text
  }
  deriving (Eq, Ord, Show)

decode :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m Instruction
decode = Parsec.between (Parsec.string' "<?") (Parsec.string' "?>") $ do
  n <- Name.decode
  v <-
    Parsec.choice
      [ do
          _ <- Parsec.many1 Parsec.blank
          decodeContent,
        Text.pack "" <$ Parsec.lookAhead (Parsec.string' "?>")
      ]
  pure $ MkInstruction n v

decodeContent :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m Text.Text
decodeContent = fmap Text.pack . Parsec.manyTill Parsec.anyChar . Parsec.lookAhead $ Parsec.string' "?>"

encode :: Instruction -> Builder.Builder
encode instr =
  Semigroup.around (Builder.stringUtf8 "<?") (Builder.stringUtf8 "?>") $
    Name.encode (name instr)
      <> if Text.null (value instr)
        then mempty
        else Builder.charUtf8 ' ' <> Builder.stringUtf8 (Text.unpack $ value instr)

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = do
  Spec.named s 'decode $ do
    Spec.it s "succeeds with empty value" $ do
      Spec.assertEq s (Parsec.parseString decode "<?foo?>") . Just $
        MkInstruction (Name.MkName $ Text.pack "foo") (Text.pack "")

    Spec.it s "succeeds with value" $ do
      Spec.assertEq s (Parsec.parseString decode "<?foo bar?>") . Just $
        MkInstruction (Name.MkName $ Text.pack "foo") (Text.pack "bar")

    Spec.it s "succeeds with xml declaration" $ do
      Spec.assertEq s (Parsec.parseString decode "<?xml version=\"1.0\"?>") . Just $
        MkInstruction (Name.MkName $ Text.pack "xml") (Text.pack "version=\"1.0\"")

    Spec.it s "succeeds with complex value" $ do
      Spec.assertEq s (Parsec.parseString decode "<?xml version=\"1.0\" encoding=\"UTF-8\"?>") . Just $
        MkInstruction (Name.MkName $ Text.pack "xml") (Text.pack "version=\"1.0\" encoding=\"UTF-8\"")

    Spec.it s "preserves multiple spaces" $ do
      Spec.assertEq s (Parsec.parseString decode "<?foo a  b?>") . Just $
        MkInstruction (Name.MkName $ Text.pack "foo") (Text.pack "a  b")

    Spec.it s "fails without closing" $ do
      Spec.assertEq s (Parsec.parseString decode "<?foo") Nothing

    Spec.it s "fails without name" $ do
      Spec.assertEq s (Parsec.parseString decode "<??>") Nothing

  Spec.named s 'encode $ do
    Spec.it s "encodes empty value" $ do
      Spec.assertEq s (Builder.toString . encode $ MkInstruction (Name.MkName $ Text.pack "foo") (Text.pack "")) "<?foo?>"

    Spec.it s "encodes with value" $ do
      Spec.assertEq s (Builder.toString . encode $ MkInstruction (Name.MkName $ Text.pack "foo") (Text.pack "bar")) "<?foo bar?>"

    Spec.it s "encodes xml declaration" $ do
      Spec.assertEq s (Builder.toString . encode $ MkInstruction (Name.MkName $ Text.pack "xml") (Text.pack "version=\"1.0\"")) "<?xml version=\"1.0\"?>"
