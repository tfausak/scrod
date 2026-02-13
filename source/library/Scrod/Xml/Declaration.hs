{-# LANGUAGE TemplateHaskellQuotes #-}

module Scrod.Xml.Declaration where

import qualified Data.ByteString.Builder as Builder
import qualified Data.Text as Text
import qualified Scrod.Extra.Builder as Builder
import qualified Scrod.Extra.Semigroup as Semigroup
import qualified Scrod.Spec as Spec
import qualified Scrod.Xml.Name as Name

-- | XML Declaration (for DOCTYPE and similar), like @\<!name value>@. Similar
-- to 'Instruction' but different delimiters. Cannot contain @>@.
data Declaration = MkDeclaration
  { name :: Name.Name,
    value :: Text.Text
  }
  deriving (Eq, Ord, Show)

encode :: Declaration -> Builder.Builder
encode decl =
  Semigroup.around (Builder.stringUtf8 "<!") (Builder.charUtf8 '>') $
    Name.encode (name decl)
      <> if Text.null (value decl)
        then mempty
        else Builder.charUtf8 ' ' <> Builder.stringUtf8 (Text.unpack $ value decl)

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = do
  Spec.named s 'encode $ do
    Spec.it s "encodes empty value" $ do
      Spec.assertEq s (Builder.toString . encode $ MkDeclaration (Name.MkName $ Text.pack "foo") (Text.pack "")) "<!foo>"

    Spec.it s "encodes with value" $ do
      Spec.assertEq s (Builder.toString . encode $ MkDeclaration (Name.MkName $ Text.pack "foo") (Text.pack "bar")) "<!foo bar>"

    Spec.it s "encodes doctype" $ do
      Spec.assertEq s (Builder.toString . encode $ MkDeclaration (Name.MkName $ Text.pack "DOCTYPE") (Text.pack "html")) "<!DOCTYPE html>"
