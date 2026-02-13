{-# LANGUAGE TemplateHaskellQuotes #-}

module Scrod.Xml.Misc where

import qualified Data.ByteString.Builder as Builder
import qualified Data.Text as Text
import qualified Scrod.Extra.Builder as Builder
import qualified Scrod.Spec as Spec
import qualified Scrod.Xml.Comment as Comment
import qualified Scrod.Xml.Declaration as Declaration
import qualified Scrod.Xml.Instruction as Instruction
import qualified Scrod.Xml.Name as Name

-- | XML Misc (what can appear in the prolog). Comments, Declarations, and
-- Processing Instructions.
data Misc
  = Comment Comment.Comment
  | Declaration Declaration.Declaration
  | Instruction Instruction.Instruction
  deriving (Eq, Ord, Show)

encode :: Misc -> Builder.Builder
encode m = case m of
  Comment c -> Comment.encode c
  Declaration d -> Declaration.encode d
  Instruction i -> Instruction.encode i

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = do
  Spec.named s 'encode $ do
    Spec.it s "encodes comment" $ do
      Spec.assertEq s (Builder.toString . encode $ Comment (Comment.MkComment $ Text.pack " hello ")) "<!-- hello -->"

    Spec.it s "encodes declaration" $ do
      Spec.assertEq s (Builder.toString . encode $ Declaration (Declaration.MkDeclaration (Name.MkName $ Text.pack "DOCTYPE") (Text.pack "html"))) "<!DOCTYPE html>"

    Spec.it s "encodes instruction" $ do
      Spec.assertEq s (Builder.toString . encode $ Instruction (Instruction.MkInstruction (Name.MkName $ Text.pack "xml") (Text.pack "version=\"1.0\""))) "<?xml version=\"1.0\"?>"
