{-# LANGUAGE TemplateHaskellQuotes #-}

module Scrod.Xml.Instruction where

import qualified Data.ByteString.Builder as Builder
import qualified Data.Text as Text
import qualified Scrod.Extra.Builder as Builder
import qualified Scrod.Extra.Semigroup as Semigroup
import qualified Scrod.Spec as Spec
import qualified Scrod.Xml.Name as Name

-- | XML Processing Instruction, like @\<?name value?>@. If value is empty, no
-- space after the name. Otherwise there must be a space after the name. Cannot
-- contain @?>@.
data Instruction = MkInstruction
  { name :: Name.Name,
    value :: Text.Text
  }
  deriving (Eq, Ord, Show)

encode :: Instruction -> Builder.Builder
encode instr =
  Semigroup.around (Builder.stringUtf8 "<?") (Builder.stringUtf8 "?>") $
    Name.encode (name instr)
      <> if Text.null (value instr)
        then mempty
        else Builder.charUtf8 ' ' <> Builder.stringUtf8 (Text.unpack $ value instr)

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = do
  Spec.named s 'encode $ do
    Spec.it s "encodes empty value" $ do
      Spec.assertEq s (Builder.toString . encode $ MkInstruction (Name.MkName $ Text.pack "foo") (Text.pack "")) "<?foo?>"

    Spec.it s "encodes with value" $ do
      Spec.assertEq s (Builder.toString . encode $ MkInstruction (Name.MkName $ Text.pack "foo") (Text.pack "bar")) "<?foo bar?>"

    Spec.it s "encodes xml declaration" $ do
      Spec.assertEq s (Builder.toString . encode $ MkInstruction (Name.MkName $ Text.pack "xml") (Text.pack "version=\"1.0\"")) "<?xml version=\"1.0\"?>"
