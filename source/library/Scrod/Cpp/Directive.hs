{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Scrod.Cpp.Directive where

import qualified Control.Monad as Monad
import qualified Scrod.Spec as Spec
import qualified Text.Parsec as Parsec

data Directive
  = If String
  | Ifdef String
  | Ifndef String
  | Elif String
  | Else
  | Endif
  | Define String (Maybe String)
  | Undef String
  | Other
  deriving (Eq, Show)

parse :: String -> Maybe Directive
parse = either (const Nothing) Just . Parsec.parse directive ""

space :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m Char
space = Parsec.oneOf " \t"

spaces :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m ()
spaces = Parsec.skipMany space

spaces1 :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m ()
spaces1 = Parsec.skipMany1 space

lexeme :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m a -> Parsec.ParsecT s u m a
lexeme = (<* spaces)

directive :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m Directive
directive = do
  spaces
  Monad.void . lexeme $ Parsec.char '#'
  keyword <- Parsec.many Parsec.letter
  case keyword of
    "if" -> If <$> rest
    "ifdef" -> Ifdef <$> name
    "ifndef" -> Ifndef <$> name
    "elif" -> Elif <$> rest
    "else" -> pure Else
    "endif" -> pure Endif
    "define" -> Define <$> name <*> value
    "undef" -> Undef <$> name
    _ -> pure Other

name :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m String
name = spaces1 *> Parsec.many1 (Parsec.choice [Parsec.alphaNum, Parsec.char '_'])

rest :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m String
rest = spaces1 *> Parsec.many Parsec.anyChar

value :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m (Maybe String)
value = Parsec.optionMaybe $ spaces1 *> Parsec.many1 Parsec.anyChar

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = do
  Spec.named s 'parse $ do
    Spec.it s "parses #if" $ do
      Spec.assertEq s (parse "#if 1") $ Just (If "1")

    Spec.it s "parses #ifdef" $ do
      Spec.assertEq s (parse "#ifdef FOO") $ Just (Ifdef "FOO")

    Spec.it s "parses #ifndef" $ do
      Spec.assertEq s (parse "#ifndef FOO") $ Just (Ifndef "FOO")

    Spec.it s "parses #elif" $ do
      Spec.assertEq s (parse "#elif 0") $ Just (Elif "0")

    Spec.it s "parses #else" $ do
      Spec.assertEq s (parse "#else") $ Just Else

    Spec.it s "parses #endif" $ do
      Spec.assertEq s (parse "#endif") $ Just Endif

    Spec.it s "parses #define without value" $ do
      Spec.assertEq s (parse "#define FOO") $ Just (Define "FOO" Nothing)

    Spec.it s "parses #define with value" $ do
      Spec.assertEq s (parse "#define FOO 42") $ Just (Define "FOO" (Just "42"))

    Spec.it s "parses #undef" $ do
      Spec.assertEq s (parse "#undef FOO") $ Just (Undef "FOO")

    Spec.it s "parses #include as Other" $ do
      Spec.assertEq s (parse "#include <stdio.h>") $ Just Other

    Spec.it s "parses #error as Other" $ do
      Spec.assertEq s (parse "#error msg") $ Just Other

    Spec.it s "handles leading whitespace" $ do
      Spec.assertEq s (parse "  #if 1") $ Just (If "1")

    Spec.it s "handles whitespace after hash" $ do
      Spec.assertEq s (parse "#  if 1") $ Just (If "1")

    Spec.it s "fails on non-directive line" $ do
      Spec.assertEq s (parse "not a directive") Nothing
