{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module LegendaryChainsaw.Css.Selector where

import qualified Data.ByteString.Builder as Builder
import qualified Data.Text as Text
import qualified LegendaryChainsaw.Extra.Builder as Builder
import qualified LegendaryChainsaw.Extra.Parsec as Parsec
import qualified LegendaryChainsaw.Spec as Spec
import qualified Text.Parsec as Parsec

-- | CSS Selector (kept as opaque text)
-- We don't parse selector internals - just capture the text.
newtype Selector = MkSelector
  { unwrap :: Text.Text
  }
  deriving (Eq, Ord, Show)

-- | Decode a selector - everything up to comma or opening brace
-- Trims leading and trailing whitespace
decode :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m Selector
decode = do
  chars <- Parsec.many1 . Parsec.satisfy $ \c -> c /= ',' && c /= '{'
  let trimmed = Text.strip . Text.pack $ chars
  if Text.null trimmed
    then fail "empty selector"
    else pure . MkSelector $ trimmed

encode :: Selector -> Builder.Builder
encode = Builder.stringUtf8 . Text.unpack . unwrap

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = do
  Spec.named s 'decode $ do
    Spec.it s "succeeds with element selector" $ do
      Spec.assertEq s (Parsec.parseString decode "div") . Just . MkSelector $ Text.pack "div"

    Spec.it s "succeeds with class selector" $ do
      Spec.assertEq s (Parsec.parseString decode ".foo") . Just . MkSelector $ Text.pack ".foo"

    Spec.it s "succeeds with id selector" $ do
      Spec.assertEq s (Parsec.parseString decode "#bar") . Just . MkSelector $ Text.pack "#bar"

    Spec.it s "succeeds with compound selector" $ do
      Spec.assertEq s (Parsec.parseString decode "div.foo#bar") . Just . MkSelector $ Text.pack "div.foo#bar"

    Spec.it s "succeeds with descendant combinator" $ do
      Spec.assertEq s (Parsec.parseString decode "div p") . Just . MkSelector $ Text.pack "div p"

    Spec.it s "succeeds with child combinator" $ do
      Spec.assertEq s (Parsec.parseString decode "ul > li") . Just . MkSelector $ Text.pack "ul > li"

    Spec.it s "succeeds with attribute selector" $ do
      Spec.assertEq s (Parsec.parseString decode "input[type=\"text\"]") . Just . MkSelector $ Text.pack "input[type=\"text\"]"

    Spec.it s "succeeds with pseudo-class" $ do
      Spec.assertEq s (Parsec.parseString decode "a:hover") . Just . MkSelector $ Text.pack "a:hover"

    Spec.it s "succeeds with pseudo-element" $ do
      Spec.assertEq s (Parsec.parseString decode "p::first-line") . Just . MkSelector $ Text.pack "p::first-line"

    Spec.it s "trims leading whitespace" $ do
      Spec.assertEq s (Parsec.parseString decode "  div") . Just . MkSelector $ Text.pack "div"

    Spec.it s "trims trailing whitespace" $ do
      Spec.assertEq s (Parsec.parseString decode "div  ") . Just . MkSelector $ Text.pack "div"

    Spec.it s "stops at comma" $ do
      Spec.assertEq s (Parsec.parseString decode "div,") . Just . MkSelector $ Text.pack "div"

    Spec.it s "stops at opening brace" $ do
      Spec.assertEq s (Parsec.parseString decode "div{") . Just . MkSelector $ Text.pack "div"

    Spec.it s "fails with empty input" $ do
      Spec.assertEq s (Parsec.parseString decode "") Nothing

    Spec.it s "fails with only whitespace" $ do
      Spec.assertEq s (Parsec.parseString decode "   ") Nothing

  Spec.named s 'encode $ do
    Spec.it s "encodes element selector" $ do
      Spec.assertEq s (Builder.toString . encode . MkSelector $ Text.pack "div") "div"

    Spec.it s "encodes complex selector" $ do
      Spec.assertEq s (Builder.toString . encode . MkSelector $ Text.pack "div.foo > p:first-child") "div.foo > p:first-child"
