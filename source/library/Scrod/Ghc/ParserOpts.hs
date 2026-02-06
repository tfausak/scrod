module Scrod.Ghc.ParserOpts where

import qualified GHC.Data.EnumSet as EnumSet
import qualified GHC.LanguageExtensions.Type as Extension
import qualified GHC.Parser.Lexer as Lexer
import qualified GHC.Utils.Error as Error

empty :: Lexer.ParserOpts
empty = fromExtensions EnumSet.empty

fromExtensions :: EnumSet.EnumSet Extension.Extension -> Lexer.ParserOpts
fromExtensions extensions =
  Lexer.mkParserOpts
    extensions
    Error.emptyDiagOpts
    False -- Are safe imports on?
    True -- Keep Haddock comments?
    True -- Keep regular comments?
    True -- Interpret line and column pragmas?
