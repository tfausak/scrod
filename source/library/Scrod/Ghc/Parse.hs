{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Parse Haskell source into a GHC AST without a full GHC session.
--
-- Bootstraps a minimal set of GHC internals ('DynFlags.empty',
-- 'ParserOpts.fromExtensions') to run the GHC lexer and parser
-- stand-alone. The pipeline is:
--
-- 1. Discover @LANGUAGE@ pragmas and extensions from the source.
-- 2. If CPP is enabled, preprocess with 'Cpp.cpp'.
-- 3. Parse into @HsModule GhcPs@ using GHC's parser.
module Scrod.Ghc.Parse where

import qualified Control.Monad.Catch as Exception
import qualified Data.Bifunctor as Bifunctor
import qualified GHC.Data.EnumSet as EnumSet
import qualified GHC.Data.FastString as FastString
import qualified GHC.Data.StringBuffer as StringBuffer
import qualified GHC.Driver.DynFlags as DynFlags
import qualified GHC.Driver.Flags as Flags
import qualified GHC.Driver.Session as Session
import qualified GHC.Hs.Extension as Ghc
import qualified GHC.LanguageExtensions.Type as Extension
import qualified GHC.Parser as Parser
import qualified GHC.Parser.Header as Header
import qualified GHC.Parser.Lexer as Lexer
import qualified GHC.Types.SourceError as SourceError
import qualified GHC.Types.SrcLoc as SrcLoc
import qualified GHC.Utils.Logger as Logger
import qualified GHC.Utils.Outputable as Outputable
import qualified Language.Haskell.Syntax as Hs
import qualified Scrod.Cabal as Cabal
import qualified Scrod.Cpp as Cpp
import qualified Scrod.Ghc.ArchOS as ArchOS
import qualified Scrod.Ghc.DynFlags as DynFlags
import qualified Scrod.Ghc.OnOff as OnOff
import qualified Scrod.Ghc.ParserOpts as ParserOpts
import qualified Scrod.Spec as Spec
import qualified System.IO.Unsafe as Unsafe

parse ::
  Bool ->
  [SrcLoc.Located String] ->
  String ->
  Either
    String
    ( (Maybe Session.Language, [DynFlags.OnOff Extension.Extension]),
      SrcLoc.Located (Hs.HsModule Ghc.GhcPs)
    )
parse isSignature extraOptions string = do
  let originalStringBuffer = StringBuffer.stringToStringBuffer string
  let cabalOptions = cabalExtensionOptions string
  languageAndExtensions <- Bifunctor.first Exception.displayException $ discoverExtensions (extraOptions <> cabalOptions) originalStringBuffer
  let extensions = uncurry resolveExtensions languageAndExtensions
  source <-
    if EnumSet.member Extension.Cpp extensions
      then Cpp.cpp string
      else Right string
  let modifiedStringBuffer = StringBuffer.stringToStringBuffer source
  let parserOpts = ParserOpts.fromExtensions extensions
  let fastString = FastString.fsLit interactiveFilePath
  let realSrcLoc = SrcLoc.mkRealSrcLoc fastString 1 1
  let pState = Lexer.initParserState parserOpts modifiedStringBuffer realSrcLoc
  let parser = if isSignature then Parser.parseSignature else Parser.parseModule
  case Lexer.unP parser pState of
    Lexer.PFailed newPState -> Left . Outputable.showSDocUnsafe . Outputable.ppr $ Lexer.getPsErrorMessages newPState
    Lexer.POk _ lHsModule -> pure (languageAndExtensions, lHsModule)

cabalExtensionOptions :: String -> [SrcLoc.Located String]
cabalExtensionOptions =
  fmap (SrcLoc.L SrcLoc.noSrcSpan . ("-X" ++)) . Cabal.discoverExtensions

discoverExtensions ::
  [SrcLoc.Located String] ->
  StringBuffer.StringBuffer ->
  Either
    SourceError.SourceError
    (Maybe Flags.Language, [DynFlags.OnOff Extension.Extension])
discoverExtensions cabalOptions =
  Unsafe.unsafePerformIO
    . Exception.try
    . discoverExtensionsIO cabalOptions

discoverExtensionsIO ::
  [SrcLoc.Located String] ->
  StringBuffer.StringBuffer ->
  IO (Maybe Flags.Language, [DynFlags.OnOff Extension.Extension])
discoverExtensionsIO cabalOptions stringBuffer = do
  logger <- Logger.initLogger
  (dynFlags, _, _) <- Session.parseDynamicFilePragma logger DynFlags.empty $ cabalOptions <> discoverOptions stringBuffer
  pure (DynFlags.language dynFlags, DynFlags.extensions dynFlags)

discoverOptions :: StringBuffer.StringBuffer -> [SrcLoc.Located String]
discoverOptions stringBuffer =
  snd $ Header.getOptions ParserOpts.empty supportedLanguages stringBuffer interactiveFilePath

supportedLanguages :: [String]
supportedLanguages = Session.supportedLanguagesAndExtensions ArchOS.empty

interactiveFilePath :: FilePath
interactiveFilePath = "<interactive>"

resolveExtensions ::
  Maybe Session.Language ->
  [DynFlags.OnOff Extension.Extension] ->
  EnumSet.EnumSet Extension.Extension
resolveExtensions =
  foldr (OnOff.onOff EnumSet.insert EnumSet.delete)
    . EnumSet.fromList
    . Session.languageExtensions

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = do
  Spec.named s 'parse $ do
    Spec.it s "succeeds with empty input" $ do
      Spec.assertEq s (fst <$> parse False [] "") $ Right (Nothing, [])

    Spec.it s "fails with invalid input" $ do
      Spec.assertEq s (fst <$> parse False [] "!") $ Left "{Resolved: ErrorWithoutFlag\n ErrorWithoutFlag\n   parse error on input `!'}"

    Spec.it s "fails with unknown language extension" $ do
      Spec.assertEq s (fst <$> parse False [] "{-# language Unknown #-}") $ Left "<interactive>:1:14: error: [GHC-46537]\n    Unsupported extension: Unknown"

    Spec.it s "succeeds with a language" $ do
      Spec.assertEq s (fst <$> parse False [] "{-# language Haskell98 #-}") $ Right (Just Session.Haskell98, [])

    Spec.it s "succeeds with an enabled extension" $ do
      Spec.assertEq s (fst <$> parse False [] "{-# language CPP #-}") $ Right (Nothing, [Session.On Extension.Cpp])

    Spec.it s "succeeds with a disabled extension" $ do
      Spec.assertEq s (fst <$> parse False [] "{-# language NoCPP #-}") $ Right (Nothing, [Session.Off Extension.Cpp])

    Spec.it s "succeeds with a signature" $ do
      Spec.assertEq s (fst <$> parse True [] "signature Foo where") $ Right (Nothing, [])

    Spec.it s "succeeds with cabal script header extension" $ do
      Spec.assertEq s (fst <$> parse False [] "{- cabal:\ndefault-extensions: CPP\n-}") $ Right (Nothing, [Session.On Extension.Cpp])

    Spec.it s "succeeds with extra options" $ do
      Spec.assertEq s (fst <$> parse False [SrcLoc.L SrcLoc.noSrcSpan "-XOverloadedStrings"] "") $ Right (Nothing, [Session.On Extension.OverloadedStrings])

    Spec.it s "succeeds with cabal script header and LANGUAGE pragma" $ do
      Spec.assertEq s (fst <$> parse False [] "{- cabal:\ndefault-extensions: CPP\n-}\n{-# language OverloadedStrings #-}") $ Right (Nothing, [Session.On Extension.OverloadedStrings, Session.On Extension.Cpp])
