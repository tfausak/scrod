{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Scrod.Xml.Misc where

import qualified Data.ByteString.Builder as Builder
import qualified Data.Text as Text
import qualified Scrod.Extra.Builder as Builder
import qualified Scrod.Extra.Parsec as Parsec
import qualified Scrod.Spec as Spec
import qualified Scrod.Xml.Comment as Comment
import qualified Scrod.Xml.Declaration as Declaration
import qualified Scrod.Xml.Instruction as Instruction
import qualified Scrod.Xml.Name as Name
import qualified Text.Parsec as Parsec

-- | XML Misc (what can appear in the prolog)
-- Comments, Declarations, and Processing Instructions
data Misc
  = Comment Comment.Comment
  | Declaration Declaration.Declaration
  | Instruction Instruction.Instruction
  deriving (Eq, Ord, Show)

decode :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m Misc
decode =
  Parsec.choice
    [ Comment <$> Comment.decode,
      Declaration <$> Declaration.decode,
      Instruction <$> Instruction.decode
    ]

encode :: Misc -> Builder.Builder
encode m = case m of
  Comment c -> Comment.encode c
  Declaration d -> Declaration.encode d
  Instruction i -> Instruction.encode i

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = do
  Spec.named s 'decode $ do
    Spec.it s "succeeds with comment" $ do
      Spec.assertEq s (Parsec.parseString decode "<!-- hello -->") . Just $
        Comment (Comment.MkComment $ Text.pack " hello ")

    Spec.it s "succeeds with declaration" $ do
      Spec.assertEq s (Parsec.parseString decode "<!DOCTYPE html>") . Just $
        Declaration (Declaration.MkDeclaration (Name.MkName $ Text.pack "DOCTYPE") (Text.pack "html"))

    Spec.it s "succeeds with instruction" $ do
      Spec.assertEq s (Parsec.parseString decode "<?xml version=\"1.0\"?>") . Just $
        Instruction (Instruction.MkInstruction (Name.MkName $ Text.pack "xml") (Text.pack "version=\"1.0\""))

    Spec.it s "fails with element" $ do
      Spec.assertEq s (Parsec.parseString decode "<foo/>") Nothing

    Spec.it s "fails with plain text" $ do
      Spec.assertEq s (Parsec.parseString decode "hello") Nothing

  Spec.named s 'encode $ do
    Spec.it s "encodes comment" $ do
      Spec.assertEq s (Builder.toString . encode $ Comment (Comment.MkComment $ Text.pack " hello ")) "<!-- hello -->"

    Spec.it s "encodes declaration" $ do
      Spec.assertEq s (Builder.toString . encode $ Declaration (Declaration.MkDeclaration (Name.MkName $ Text.pack "DOCTYPE") (Text.pack "html"))) "<!DOCTYPE html>"

    Spec.it s "encodes instruction" $ do
      Spec.assertEq s (Builder.toString . encode $ Instruction (Instruction.MkInstruction (Name.MkName $ Text.pack "xml") (Text.pack "version=\"1.0\""))) "<?xml version=\"1.0\"?>"
