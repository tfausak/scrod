module Scrod.Unstable.Type.Json
  ( -- * Types
    Json (..),

    -- * Parsing
    parse,

    -- * Rendering
    toBuilder,
    render,

    -- * Construction helpers
    tag,
    tagged,
    object,
    fromNatural,
    fromInt,
    integerToJson,
    fromText,
    fromBool,
    fromList,
    fromMap,
  )
where

import Control.Monad (void)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Char as Char
import Data.Functor (($>))
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Numeric
import qualified Numeric.Natural as Natural
import qualified Scrod.Unstable.Type.Decimal as Decimal
import qualified Text.Parsec as Parsec

-- * Types

-- | A JSON value as described by RFC 8259.
data Json
  = Null
  | Boolean Bool
  | Number Decimal.Decimal
  | String Text.Text
  | Array [Json]
  | Object (Map.Map Text.Text Json)
  deriving (Eq, Ord, Show)

-- * Parsing

-- | Parse JSON text into a Json value.
parse :: Text.Text -> Either Parsec.ParseError Json
parse = Parsec.runParser (pJson <* Parsec.eof) () ""

type Parser = Parsec.Parsec Text.Text ()

pJson :: Parser Json
pJson = do
  pWhitespace
  pValue

pValue :: Parser Json
pValue =
  Parsec.choice
    [ pNull,
      pBoolean,
      pNumber,
      pString,
      pArray,
      pObject
    ]

pNull :: Parser Json
pNull = do
  void $ Parsec.string "null"
  pWhitespace
  pure Null

pBoolean :: Parser Json
pBoolean = pTrue Parsec.<|> pFalse

pTrue :: Parser Json
pTrue = do
  void $ Parsec.string "true"
  pWhitespace
  pure $ Boolean True

pFalse :: Parser Json
pFalse = do
  void $ Parsec.string "false"
  pWhitespace
  pure $ Boolean False

pNumber :: Parser Json
pNumber = do
  sign <- Parsec.option 1 (Parsec.char '-' $> (-1))
  intPart <- pIntegerPart
  (fracDigits, fracLen) <- Parsec.option (0, 0) pFraction
  expPart <- Parsec.option 0 pExponent
  pWhitespace
  let coef = sign * (intPart * 10 ^ fracLen + fracDigits)
  let expo = expPart - fracLen
  pure . Number $ Decimal.MkDecimal coef expo

pIntegerPart :: Parser Integer
pIntegerPart =
  Parsec.try (Parsec.char '0' $> 0) Parsec.<|> pNonZero

pNonZero :: Parser Integer
pNonZero = do
  first <- Parsec.satisfy isNonZeroDigit
  rest <- Parsec.many $ Parsec.satisfy Char.isDigit
  pure $ read (first : rest)

pFraction :: Parser (Integer, Integer)
pFraction = do
  void $ Parsec.char '.'
  digits <- Parsec.many1 $ Parsec.satisfy Char.isDigit
  pure (read digits, fromIntegral $ length digits)

pExponent :: Parser Integer
pExponent = do
  void $ Parsec.char 'e' Parsec.<|> Parsec.char 'E'
  sign <- Parsec.option 1 $ (Parsec.char '+' $> 1) Parsec.<|> (Parsec.char '-' $> (-1))
  digits <- Parsec.many1 $ Parsec.satisfy Char.isDigit
  pure $ sign * read digits

pString :: Parser Json
pString = do
  void $ Parsec.char '"'
  chars <- Parsec.many pCharacter
  void $ Parsec.char '"'
  pWhitespace
  pure . String $ Text.pack chars

pCharacter :: Parser Char
pCharacter = pLiteral Parsec.<|> pEscape

pLiteral :: Parser Char
pLiteral = Parsec.satisfy isLiteral

pEscape :: Parser Char
pEscape = do
  void $ Parsec.char '\\'
  escape <- Parsec.anyChar
  case escape of
    '"' -> pure '"'
    '\\' -> pure '\\'
    '/' -> pure '/'
    'b' -> pure '\b'
    'f' -> pure '\f'
    'n' -> pure '\n'
    'r' -> pure '\r'
    't' -> pure '\t'
    'u' -> do
      hex <- Parsec.count 4 $ Parsec.satisfy Char.isHexDigit
      case Numeric.readHex hex of
        [(n, "")] -> pure $ Char.chr n
        _ -> Parsec.unexpected "invalid unicode escape"
    _ -> Parsec.unexpected "invalid escape sequence"

pArray :: Parser Json
pArray = do
  void $ Parsec.char '['
  pWhitespace
  values <- Parsec.sepBy pValue (Parsec.char ',' >> pWhitespace)
  void $ Parsec.char ']'
  pWhitespace
  pure $ Array values

pObject :: Parser Json
pObject = do
  void $ Parsec.char '{'
  pWhitespace
  pairs <- Parsec.sepBy pPair (Parsec.char ',' >> pWhitespace)
  void $ Parsec.char '}'
  pWhitespace
  pure . Object $ Map.fromList pairs

pPair :: Parser (Text.Text, Json)
pPair = do
  String key <- pString
  void $ Parsec.char ':'
  pWhitespace
  value <- pValue
  pure (key, value)

pWhitespace :: Parser ()
pWhitespace = void . Parsec.many $ Parsec.satisfy isWhitespace

isWhitespace :: Char -> Bool
isWhitespace c = c == ' ' || c == '\t' || c == '\n' || c == '\r'

isNonZeroDigit :: Char -> Bool
isNonZeroDigit c = c >= '1' && c <= '9'

isLiteral :: Char -> Bool
isLiteral c = c /= '"' && c /= '\\' && c >= '\x20'

-- * Rendering

-- | Render a Json value to a lazy ByteString.
render :: Json -> LazyByteString.ByteString
render = Builder.toLazyByteString . toBuilder

-- | Render a Json value to a ByteString Builder.
toBuilder :: Json -> Builder.Builder
toBuilder json = case json of
  Null -> Builder.stringUtf8 "null"
  Boolean b -> if b then Builder.stringUtf8 "true" else Builder.stringUtf8 "false"
  Number d -> sDecimal d
  String t -> sString t
  Array xs -> sArray xs
  Object m -> sObject m

sDecimal :: Decimal.Decimal -> Builder.Builder
sDecimal (Decimal.MkDecimal coef expo) =
  Builder.integerDec coef <> Builder.char7 'e' <> Builder.integerDec expo

sString :: Text.Text -> Builder.Builder
sString t =
  Builder.char7 '"'
    <> Text.foldl' (\acc c -> acc <> sChar c) mempty t
    <> Builder.char7 '"'

sChar :: Char -> Builder.Builder
sChar c = case c of
  '"' -> Builder.stringUtf8 "\\\""
  '\\' -> Builder.stringUtf8 "\\\\"
  '\b' -> Builder.stringUtf8 "\\b"
  '\f' -> Builder.stringUtf8 "\\f"
  '\n' -> Builder.stringUtf8 "\\n"
  '\r' -> Builder.stringUtf8 "\\r"
  '\t' -> Builder.stringUtf8 "\\t"
  _ ->
    if Char.ord c < 0x20
      then Builder.stringUtf8 "\\u" <> Builder.stringUtf8 (padHex 4 (Char.ord c))
      else Builder.charUtf8 c

padHex :: Int -> Int -> String
padHex n x =
  let hex = Numeric.showHex x ""
   in replicate (n - length hex) '0' <> hex

sArray :: [Json] -> Builder.Builder
sArray xs =
  Builder.char7 '['
    <> mconcat (List.intersperse (Builder.char7 ',') (fmap toBuilder xs))
    <> Builder.char7 ']'

sObject :: Map.Map Text.Text Json -> Builder.Builder
sObject m =
  Builder.char7 '{'
    <> mconcat (List.intersperse (Builder.char7 ',') (fmap sPair (Map.toList m)))
    <> Builder.char7 '}'

sPair :: (Text.Text, Json) -> Builder.Builder
sPair (key, value) = sString key <> Builder.char7 ':' <> toBuilder value

-- * Construction helpers

-- | Create a tagged object for sum types without contents.
-- Example: @tag "Value"@ produces @{"tag": "Value"}@
tag :: Text.Text -> Json
tag t = Object $ Map.singleton (Text.pack "tag") (String t)

-- | Create a tagged object with contents.
-- Example: @tagged "Some" (Number 42)@ produces @{"tag": "Some", "contents": 42}@
tagged :: Text.Text -> Json -> Json
tagged t contents =
  Object $
    Map.fromList
      [ (Text.pack "tag", String t),
        (Text.pack "contents", contents)
      ]

-- | Create a JSON object from a list of key-value pairs.
object :: [(Text.Text, Json)] -> Json
object = Object . Map.fromList

-- | Convert a Natural to a JSON Number.
fromNatural :: Natural.Natural -> Json
fromNatural n = Number $ Decimal.MkDecimal (toInteger n) 0

-- | Convert an Int to a JSON Number.
fromInt :: Int -> Json
fromInt n = Number $ Decimal.MkDecimal (toInteger n) 0

-- | Convert an Integer to a JSON Number.
integerToJson :: Integer -> Json
integerToJson n = Number $ Decimal.MkDecimal n 0

-- | Convert Text to a JSON String.
fromText :: Text.Text -> Json
fromText = String

-- | Convert Bool to a JSON Boolean.
fromBool :: Bool -> Json
fromBool = Boolean

-- | Convert a list to a JSON Array.
fromList :: [Json] -> Json
fromList = Array

-- | Convert a Map to a JSON Object.
fromMap :: Map.Map Text.Text Json -> Json
fromMap = Object
