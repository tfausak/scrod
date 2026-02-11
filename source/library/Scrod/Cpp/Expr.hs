{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Evaluation of C preprocessor @#if@ \/ @#elif@ expressions.
--
-- Supports integer literals (decimal and hex), arithmetic operators,
-- comparisons, logical operators, the @defined@ operator, macro name
-- lookup, and parenthesized sub-expressions. Undefined identifiers and
-- unrecognized function-like macro calls evaluate to @0@, matching
-- standard CPP behavior.
module Scrod.Cpp.Expr where

import qualified Data.Bool as Bool
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Scrod.Cpp.Directive as Directive
import qualified Scrod.Extra.Read as Read
import qualified Scrod.Spec as Spec
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Expr as Expr

-- | Evaluate a CPP expression string in the context of a set of @#define@d
-- macros. Returns 'Left' on parse errors or division by zero.
evaluate :: Map.Map String String -> String -> Either String Integer
evaluate defines input =
  case Parsec.parse (Directive.spaces *> expression defines <* Parsec.eof) "" input of
    Left err -> Left $ show err
    Right (Left msg) -> Left msg
    Right (Right n) -> Right n

expression :: (Parsec.Stream s m Char) => Map.Map String String -> Parsec.ParsecT s u m (Either String Integer)
expression = Expr.buildExpressionParser operatorTable . term

-- | Operator precedence table, from highest to lowest:
-- @*@\/@\/@\/@%@, @+@\/@-@, comparisons, equality, @&&@, @||@.
operatorTable :: (Parsec.Stream s m Char) => Expr.OperatorTable s u m (Either String Integer)
operatorTable =
  [ [ infixL "*" $ liftA2 (*),
      infixL "/" checkedDiv,
      infixL "%" checkedMod
    ],
    [ infixL "+" $ liftA2 (+),
      infixL "-" $ liftA2 (-)
    ],
    [ infixL "<=" $ liftA2 (boolOp (<=)),
      infixL ">=" $ liftA2 (boolOp (>=)),
      infixL "<" $ liftA2 (boolOp (<)),
      infixL ">" $ liftA2 (boolOp (>))
    ],
    [ infixL "==" $ liftA2 (boolOp (==)),
      infixL "!=" $ liftA2 (boolOp (/=))
    ],
    [ infixL "&&" $ liftA2 (\a b -> boolToInt (a /= 0 && b /= 0))
    ],
    [ infixL "||" $ liftA2 (\a b -> boolToInt (a /= 0 || b /= 0))
    ]
  ]

infixL ::
  (Parsec.Stream s m Char) =>
  String ->
  (Either String Integer -> Either String Integer -> Either String Integer) ->
  Expr.Operator s u m (Either String Integer)
infixL s f =
  Expr.Infix
    (f <$ Directive.lexeme (Parsec.string' s))
    Expr.AssocLeft

-- | Parse a term: unary operators (@!@, @-@, @+@), integer literals,
-- @defined@ expressions, identifiers (with optional function-call syntax),
-- or parenthesized sub-expressions.
term :: (Parsec.Stream s m Char) => Map.Map String String -> Parsec.ParsecT s u m (Either String Integer)
term defines =
  Parsec.choice
    [ do
        _ <- Directive.lexeme $ Parsec.char '!'
        n <- term defines
        pure $ fmap (boolToInt . (== 0)) n,
      do
        _ <- Directive.lexeme $ Parsec.char '-'
        n <- term defines
        pure $ fmap negate n,
      do
        _ <- Directive.lexeme $ Parsec.char '+'
        term defines,
      Right <$> Directive.lexeme intLiteral,
      Right <$> Directive.lexeme (definedExpr defines),
      Right <$> Directive.lexeme (identifier defines),
      Parsec.between (Directive.lexeme $ Parsec.char '(') (Directive.lexeme $ Parsec.char ')') $
        expression defines
    ]

checkedDiv :: Either String Integer -> Either String Integer -> Either String Integer
checkedDiv a b = case b of
  Right 0 -> Left "division by zero in #if"
  _ -> liftA2 div a b

checkedMod :: Either String Integer -> Either String Integer -> Either String Integer
checkedMod a b = case b of
  Right 0 -> Left "division by zero in #if"
  _ -> liftA2 mod a b

boolOp :: (Integer -> Integer -> Bool) -> Integer -> Integer -> Integer
boolOp f x = boolToInt . f x

intLiteral :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m Integer
intLiteral = Parsec.choice [Parsec.try hexLiteral, decLiteral]

hexLiteral :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m Integer
hexLiteral = do
  _ <- Parsec.char '0'
  _ <- Parsec.oneOf "xX"
  digits <- Parsec.many1 Parsec.hexDigit
  maybe (fail "invalid hexadecimal literal") pure . Read.readM $ "0x" <> digits

decLiteral :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m Integer
decLiteral = do
  digits <- Parsec.many1 Parsec.digit
  maybe (fail "invalid decimal literal") pure $ Read.readM digits

-- | Parse a @defined NAME@ or @defined(NAME)@ expression. Returns @1@ if the
-- name is in the defines map, @0@ otherwise.
definedExpr :: (Parsec.Stream s m Char) => Map.Map String String -> Parsec.ParsecT s u m Integer
definedExpr defines = do
  _ <- Parsec.try $ Parsec.string "defined" <* Parsec.notFollowedBy (Parsec.choice [Parsec.alphaNum, Parsec.char '_'])
  Directive.spaces
  n <-
    Directive.lexeme $
      Parsec.choice
        [ Parsec.try
            . Parsec.between
              (Directive.lexeme $ Parsec.char '(')
              (Parsec.char ')')
            $ Directive.lexeme identName,
          identName
        ]
  pure . boolToInt $ Map.member n defines

identName :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m String
identName = do
  c <- Parsec.choice [Parsec.letter, Parsec.char '_']
  cs <- Parsec.many (Parsec.choice [Parsec.alphaNum, Parsec.char '_'])
  pure (c : cs)

-- | Parse an identifier and look up its value in the defines map. If followed
-- by parenthesized arguments (function-like macro call), returns @0@.
-- Undefined or non-numeric identifiers also evaluate to @0@.
identifier :: (Parsec.Stream s m Char) => Map.Map String String -> Parsec.ParsecT s u m Integer
identifier defines = do
  n <- identName
  -- Handle undefined function-like macro calls: NAME(...) -> 0
  mParen <- Parsec.optionMaybe $ Parsec.char '(' *> consumeBalancedParens 1
  pure $ case mParen of
    Just () -> 0
    Nothing -> Maybe.fromMaybe 0 $ do
      x <- Map.lookup n defines
      Read.readM x

-- | Skip input until parentheses are balanced. The initial depth should be
-- @1@ (after consuming the opening @(@).
consumeBalancedParens :: (Parsec.Stream s m Char) => Int -> Parsec.ParsecT s u m ()
consumeBalancedParens depth = case depth of
  0 -> pure ()
  _ -> do
    c <- Parsec.anyChar
    case c of
      '(' -> consumeBalancedParens (depth + 1)
      ')' -> consumeBalancedParens (depth - 1)
      _ -> consumeBalancedParens depth

boolToInt :: Bool -> Integer
boolToInt = Bool.bool 0 1

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
