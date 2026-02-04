{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module LegendaryChainsaw.JsonPointer.Pointer where

import qualified Data.ByteString.Builder as Builder
import qualified Data.Text as Text
import qualified LegendaryChainsaw.Extra.Builder as Builder
import qualified LegendaryChainsaw.Extra.Parsec as Parsec
import qualified LegendaryChainsaw.JsonPointer.Token as Token
import qualified LegendaryChainsaw.Spec as Spec
import qualified Text.Parsec as Parsec

-- | A JSON Pointer as defined by RFC 6901.
-- A JSON Pointer is a sequence of zero or more reference tokens.
-- An empty list represents the root of the document.
newtype Pointer = MkPointer
  { unwrap :: [Token.Token]
  } deriving (Eq, Ord, Show)

-- | Decodes a JSON Pointer from a string.
-- Per RFC 6901, a JSON Pointer is either an empty string (root)
-- or a sequence of reference tokens each prefixed by '/'.
decode :: Parsec.Stream s m Char => Parsec.ParsecT s u m Pointer
decode = MkPointer <$> Parsec.many (Parsec.char '/' *> Token.decode)

-- | Encodes a JSON Pointer to a string.
-- Each token is prefixed with '/'.
encode :: Pointer -> Builder.Builder
encode = foldMap encodeToken . unwrap

encodeToken :: Token.Token -> Builder.Builder
encodeToken t = Builder.char8 '/' <> Token.encode t

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = do
  let pointer = MkPointer . fmap (Token.MkToken . Text.pack)

  Spec.named s 'decode $ do
    Spec.it s "succeeds with empty string (root pointer)" $ do
      Spec.assertEq s (Parsec.parseString decode "") . Just $ pointer []

    Spec.it s "succeeds with single slash (empty token)" $ do
      Spec.assertEq s (Parsec.parseString decode "/") . Just $ pointer [""]

    Spec.it s "succeeds with /foo" $ do
      Spec.assertEq s (Parsec.parseString decode "/foo") . Just $ pointer ["foo"]

    Spec.it s "succeeds with /foo/bar" $ do
      Spec.assertEq s (Parsec.parseString decode "/foo/bar") . Just $ pointer ["foo", "bar"]

    Spec.it s "succeeds with /foo/0" $ do
      Spec.assertEq s (Parsec.parseString decode "/foo/0") . Just $ pointer ["foo", "0"]

    Spec.it s "succeeds with /a~1b (contains /)" $ do
      Spec.assertEq s (Parsec.parseString decode "/a~1b") . Just $ pointer ["a/b"]

    Spec.it s "succeeds with /m~0n (contains ~)" $ do
      Spec.assertEq s (Parsec.parseString decode "/m~0n") . Just $ pointer ["m~n"]

    Spec.it s "succeeds with /c%d (contains percent)" $ do
      Spec.assertEq s (Parsec.parseString decode "/c%d") . Just $ pointer ["c%d"]

    Spec.it s "succeeds with /e^f (contains caret)" $ do
      Spec.assertEq s (Parsec.parseString decode "/e^f") . Just $ pointer ["e^f"]

    Spec.it s "succeeds with /g|h (contains pipe)" $ do
      Spec.assertEq s (Parsec.parseString decode "/g|h") . Just $ pointer ["g|h"]

    Spec.it s "succeeds with /i\\\\j (contains backslash)" $ do
      Spec.assertEq s (Parsec.parseString decode "/i\\j") . Just $ pointer ["i\\j"]

    Spec.it s "succeeds with /k\"l (contains quote)" $ do
      Spec.assertEq s (Parsec.parseString decode "/k\"l") . Just $ pointer ["k\"l"]

    Spec.it s "succeeds with / / (contains space)" $ do
      Spec.assertEq s (Parsec.parseString decode "/ ") . Just $ pointer [" "]

    Spec.it s "handles multiple empty tokens" $ do
      Spec.assertEq s (Parsec.parseString decode "///") . Just $ pointer ["", "", ""]

  Spec.named s 'encode $ do
    Spec.it s "works with empty pointer (root)" $ do
      Spec.assertEq s (Builder.toString . encode $ pointer []) ""

    Spec.it s "works with single empty token" $ do
      Spec.assertEq s (Builder.toString . encode $ pointer [""]) "/"

    Spec.it s "works with /foo" $ do
      Spec.assertEq s (Builder.toString . encode $ pointer ["foo"]) "/foo"

    Spec.it s "works with /foo/bar" $ do
      Spec.assertEq s (Builder.toString . encode $ pointer ["foo", "bar"]) "/foo/bar"

    Spec.it s "works with /foo/0" $ do
      Spec.assertEq s (Builder.toString . encode $ pointer ["foo", "0"]) "/foo/0"

    Spec.it s "encodes / in token as ~1" $ do
      Spec.assertEq s (Builder.toString . encode $ pointer ["a/b"]) "/a~1b"

    Spec.it s "encodes ~ in token as ~0" $ do
      Spec.assertEq s (Builder.toString . encode $ pointer ["m~n"]) "/m~0n"

    Spec.it s "does not escape percent" $ do
      Spec.assertEq s (Builder.toString . encode $ pointer ["c%d"]) "/c%d"

    Spec.it s "handles multiple empty tokens" $ do
      Spec.assertEq s (Builder.toString . encode $ pointer ["", "", ""]) "///"
