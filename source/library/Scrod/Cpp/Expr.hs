{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Scrod.Cpp.Expr where

import qualified Data.Char as Char
import qualified Data.Functor as Functor
import qualified Data.Map.Strict as Map
import qualified Scrod.Spec as Spec
import qualified Text.Parsec as Parsec

evaluate :: Map.Map String String -> String -> Either String Integer
evaluate defines input =
  case Parsec.parse (spaces *> expr defines <* Parsec.eof) "" input of
    Left err -> Left $ show err
    Right n -> Right n

spaces :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m ()
spaces = Parsec.skipMany (Parsec.oneOf " \t")

lexeme :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m a -> Parsec.ParsecT s u m a
lexeme p = p <* spaces

expr :: (Parsec.Stream s m Char) => Map.Map String String -> Parsec.ParsecT s u m Integer
expr = orExpr

orExpr :: (Parsec.Stream s m Char) => Map.Map String String -> Parsec.ParsecT s u m Integer
orExpr defines = do
  l <- andExpr defines
  rs <- Parsec.many $ do
    _ <- lexeme $ Parsec.try (Parsec.string "||")
    andExpr defines
  pure $ foldl (\a b -> if a /= 0 || b /= 0 then 1 else 0) l rs

andExpr :: (Parsec.Stream s m Char) => Map.Map String String -> Parsec.ParsecT s u m Integer
andExpr defines = do
  l <- eqExpr defines
  rs <- Parsec.many $ do
    _ <- lexeme $ Parsec.try (Parsec.string "&&")
    eqExpr defines
  pure $ foldl (\a b -> if a /= 0 && b /= 0 then 1 else 0) l rs

eqExpr :: (Parsec.Stream s m Char) => Map.Map String String -> Parsec.ParsecT s u m Integer
eqExpr defines = do
  l <- relExpr defines
  rs <- Parsec.many $ do
    op <-
      lexeme $
        Parsec.try (Parsec.string "==" Functor.$> (==))
          Parsec.<|> Parsec.try (Parsec.string "!=" Functor.$> (/=))
    r <- relExpr defines
    pure (op, r)
  pure $ foldl (\a (op, b) -> boolToInt $ op a b) l rs

relExpr :: (Parsec.Stream s m Char) => Map.Map String String -> Parsec.ParsecT s u m Integer
relExpr defines = do
  l <- addExpr defines
  rs <- Parsec.many $ do
    op <-
      lexeme $
        Parsec.try (Parsec.string "<=" Functor.$> (<=))
          Parsec.<|> Parsec.try (Parsec.string ">=" Functor.$> (>=))
          Parsec.<|> Parsec.try (Parsec.string "<" Functor.$> (<))
          Parsec.<|> Parsec.try (Parsec.string ">" Functor.$> (>))
    r <- addExpr defines
    pure (op, r)
  pure $ foldl (\a (op, b) -> boolToInt $ op a b) l rs

addExpr :: (Parsec.Stream s m Char) => Map.Map String String -> Parsec.ParsecT s u m Integer
addExpr defines = do
  l <- mulExpr defines
  rs <- Parsec.many $ do
    op <-
      lexeme $
        Parsec.try (Parsec.char '+' Functor.$> (+))
          Parsec.<|> Parsec.try (Parsec.char '-' Functor.$> (-))
    r <- mulExpr defines
    pure (op, r)
  pure $ foldl (\a (op, b) -> op a b) l rs

mulExpr :: (Parsec.Stream s m Char) => Map.Map String String -> Parsec.ParsecT s u m Integer
mulExpr defines = do
  l <- unaryExpr defines
  rs <- Parsec.many $ do
    op <-
      lexeme $
        Parsec.try (Parsec.char '*')
          Parsec.<|> Parsec.try (Parsec.char '/')
          Parsec.<|> Parsec.try (Parsec.char '%')
    r <- unaryExpr defines
    pure (op, r)
  applyMulOps l rs

applyMulOps :: (Monad m) => Integer -> [(Char, Integer)] -> Parsec.ParsecT s u m Integer
applyMulOps acc [] = pure acc
applyMulOps acc ((op, b) : rest) = case op of
  '*' -> applyMulOps (acc * b) rest
  '/' | b == 0 -> fail "division by zero in #if"
  '/' -> applyMulOps (div acc b) rest
  _ | b == 0 -> fail "division by zero in #if"
  _ -> applyMulOps (mod acc b) rest

unaryExpr :: (Parsec.Stream s m Char) => Map.Map String String -> Parsec.ParsecT s u m Integer
unaryExpr defines =
  Parsec.choice
    [ do
        _ <- lexeme $ Parsec.char '!'
        n <- unaryExpr defines
        pure $ if n == 0 then 1 else 0,
      do
        _ <- lexeme $ Parsec.char '-'
        n <- unaryExpr defines
        pure $ negate n,
      do
        _ <- lexeme $ Parsec.char '+'
        unaryExpr defines,
      primary defines
    ]

primary :: (Parsec.Stream s m Char) => Map.Map String String -> Parsec.ParsecT s u m Integer
primary defines =
  Parsec.choice
    [ lexeme intLiteral,
      lexeme $ definedExpr defines,
      lexeme $ identifier defines,
      do
        _ <- lexeme $ Parsec.char '('
        n <- expr defines
        _ <- lexeme $ Parsec.char ')'
        pure n
    ]

intLiteral :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m Integer
intLiteral = Parsec.try hexLiteral Parsec.<|> decLiteral

hexLiteral :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m Integer
hexLiteral = do
  _ <- Parsec.char '0'
  _ <- Parsec.oneOf "xX"
  digits <- Parsec.many1 Parsec.hexDigit
  pure $ foldl (\acc d -> acc * 16 + fromIntegral (Char.digitToInt d)) 0 digits

decLiteral :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m Integer
decLiteral = do
  digits <- Parsec.many1 Parsec.digit
  pure $ read digits

definedExpr :: (Parsec.Stream s m Char) => Map.Map String String -> Parsec.ParsecT s u m Integer
definedExpr defines = do
  _ <- Parsec.try $ Parsec.string "defined" <* Parsec.notFollowedBy (Parsec.alphaNum Parsec.<|> Parsec.char '_')
  spaces
  parenned <-
    Parsec.optionMaybe . Parsec.try $ do
      _ <- Parsec.char '('
      spaces
      n <- identName
      spaces
      _ <- Parsec.char ')'
      pure n
  case parenned of
    Just n -> pure $ if Map.member n defines then 1 else 0
    Nothing -> do
      n <- identName
      pure $ if Map.member n defines then 1 else 0

identName :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m String
identName = do
  c <- Parsec.letter Parsec.<|> Parsec.char '_'
  cs <- Parsec.many (Parsec.alphaNum Parsec.<|> Parsec.char '_')
  pure (c : cs)

identifier :: (Parsec.Stream s m Char) => Map.Map String String -> Parsec.ParsecT s u m Integer
identifier defines = do
  n <- identName
  -- Handle undefined function-like macro calls: NAME(...) -> 0
  mParen <- Parsec.optionMaybe . Parsec.try $ do
    _ <- Parsec.char '('
    consumeBalancedParens 1
  case mParen of
    Just () -> pure 0
    Nothing ->
      case Map.lookup n defines of
        Nothing -> pure 0
        Just v -> case reads v of
          [(i, "")] -> pure i
          _ -> pure 0

consumeBalancedParens :: (Parsec.Stream s m Char) => Int -> Parsec.ParsecT s u m ()
consumeBalancedParens 0 = pure ()
consumeBalancedParens depth = do
  c <- Parsec.anyChar
  case c of
    '(' -> consumeBalancedParens (depth + 1)
    ')' -> consumeBalancedParens (depth - 1)
    _ -> consumeBalancedParens depth

boolToInt :: Bool -> Integer
boolToInt b = if b then 1 else 0

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = do
  Spec.named s 'evaluate $ do
    Spec.it s "evaluates integer literal" $ do
      Spec.assertEq s (evaluate Map.empty "42") $ Right 42

    Spec.it s "evaluates zero" $ do
      Spec.assertEq s (evaluate Map.empty "0") $ Right 0

    Spec.it s "evaluates hexadecimal literal" $ do
      Spec.assertEq s (evaluate Map.empty "0x1F") $ Right 31

    Spec.it s "evaluates undefined name as 0" $ do
      Spec.assertEq s (evaluate Map.empty "FOO") $ Right 0

    Spec.it s "evaluates defined name" $ do
      Spec.assertEq s (evaluate (Map.singleton "FOO" "1") "FOO") $ Right 1

    Spec.it s "evaluates defined name with numeric value" $ do
      Spec.assertEq s (evaluate (Map.singleton "FOO" "5") "FOO") $ Right 5

    Spec.it s "evaluates defined name with non-numeric value as 0" $ do
      Spec.assertEq s (evaluate (Map.singleton "FOO" "bar") "FOO") $ Right 0

    Spec.it s "evaluates defined(NAME) with parens" $ do
      Spec.assertEq s (evaluate (Map.singleton "FOO" "1") "defined(FOO)") $ Right 1

    Spec.it s "evaluates defined(NAME) for undefined" $ do
      Spec.assertEq s (evaluate Map.empty "defined(FOO)") $ Right 0

    Spec.it s "evaluates defined NAME without parens" $ do
      Spec.assertEq s (evaluate (Map.singleton "FOO" "1") "defined FOO") $ Right 1

    Spec.it s "evaluates logical not of zero" $ do
      Spec.assertEq s (evaluate Map.empty "!0") $ Right 1

    Spec.it s "evaluates logical not of nonzero" $ do
      Spec.assertEq s (evaluate Map.empty "!5") $ Right 0

    Spec.it s "evaluates logical or" $ do
      Spec.assertEq s (evaluate Map.empty "0 || 1") $ Right 1

    Spec.it s "evaluates logical and" $ do
      Spec.assertEq s (evaluate Map.empty "1 && 0") $ Right 0

    Spec.it s "evaluates equality" $ do
      Spec.assertEq s (evaluate Map.empty "1 == 1") $ Right 1

    Spec.it s "evaluates inequality" $ do
      Spec.assertEq s (evaluate Map.empty "1 != 2") $ Right 1

    Spec.it s "evaluates less than" $ do
      Spec.assertEq s (evaluate Map.empty "2 < 3") $ Right 1

    Spec.it s "evaluates greater than" $ do
      Spec.assertEq s (evaluate Map.empty "3 > 2") $ Right 1

    Spec.it s "evaluates addition" $ do
      Spec.assertEq s (evaluate Map.empty "2 + 3") $ Right 5

    Spec.it s "evaluates subtraction" $ do
      Spec.assertEq s (evaluate Map.empty "5 - 3") $ Right 2

    Spec.it s "evaluates multiplication" $ do
      Spec.assertEq s (evaluate Map.empty "3 * 4") $ Right 12

    Spec.it s "evaluates parenthesized expression" $ do
      Spec.assertEq s (evaluate Map.empty "(1 + 2) * 3") $ Right 9

    Spec.it s "evaluates complex expression" $ do
      Spec.assertEq s (evaluate (Map.singleton "X" "10") "defined(X) && X >= 10") $ Right 1

    Spec.it s "evaluates unary minus" $ do
      Spec.assertEq s (evaluate Map.empty "-1") $ Right (-1)

    Spec.it s "handles undefined function-like macro call" $ do
      Spec.assertEq s (evaluate Map.empty "MIN_VERSION_base(4,16,0)") $ Right 0

    Spec.it s "evaluates addition without spaces" $ do
      Spec.assertEq s (evaluate Map.empty "1+2") $ Right 3

    Spec.it s "evaluates subtraction without spaces" $ do
      Spec.assertEq s (evaluate Map.empty "5-3") $ Right 2

    Spec.it s "fails on division by zero" $ do
      case evaluate Map.empty "1/0" of
        Left _ -> pure ()
        Right _ -> Spec.assertFailure s "expected failure on division by zero"

    Spec.it s "fails on modulo by zero" $ do
      case evaluate Map.empty "1%0" of
        Left _ -> pure ()
        Right _ -> Spec.assertFailure s "expected failure on modulo by zero"

    Spec.it s "treats identifiers starting with defined as identifiers" $ do
      Spec.assertEq s (evaluate (Map.singleton "definedFoo" "5") "definedFoo") $ Right 5

    Spec.it s "fails on empty expression" $ do
      case evaluate Map.empty "" of
        Left _ -> pure ()
        Right _ -> Spec.assertFailure s "expected failure on empty expression"
