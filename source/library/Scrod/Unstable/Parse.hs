module Scrod.Unstable.Parse where

-- <https://downloads.haskell.org/~ghc/9.10.3/docs/libraries/ghc-9.10.3-3614/index.html>

import qualified Control.Exception as Exception
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
import qualified GHC.Parser.Errors.Types as Errors
import qualified GHC.Parser.Header as Header
import qualified GHC.Parser.Lexer as Lexer
import qualified GHC.Types.Error as Error
import qualified GHC.Types.SourceError as SourceError
import qualified GHC.Types.SrcLoc as SrcLoc
import qualified Language.Haskell.Syntax as Hs
import qualified Scrod.Unstable.Extra.DynFlags as DynFlags
import qualified Scrod.Unstable.Extra.OnOff as OnOff
import qualified Scrod.Unstable.Extra.ParserOpts as ParserOpts
import qualified System.IO.Unsafe as Unsafe

parse ::
  String ->
  Either
    (Either SourceError.SourceError (Error.Messages Errors.PsMessage))
    ( (Maybe Session.Language, [DynFlags.OnOff Extension.Extension]),
      SrcLoc.Located (Hs.HsModule Ghc.GhcPs)
    )
parse string = do
  let stringBuffer = StringBuffer.stringToStringBuffer string
  languageAndExtensions <- Bifunctor.first Left $ discoverExtensions stringBuffer
  let parserOpts = ParserOpts.fromExtensions $ uncurry resolveExtensions languageAndExtensions
  let fastString = FastString.fsLit interactiveFilePath
  let realSrcLoc = SrcLoc.mkRealSrcLoc fastString 1 1
  let pState = Lexer.initParserState parserOpts stringBuffer realSrcLoc
  case Lexer.unP Parser.parseModule pState of
    Lexer.PFailed newPState -> Left . Right $ Lexer.getPsErrorMessages newPState
    Lexer.POk _ lHsModule -> pure (languageAndExtensions, lHsModule)

discoverExtensions ::
  StringBuffer.StringBuffer ->
  Either
    SourceError.SourceError
    (Maybe Flags.Language, [DynFlags.OnOff Extension.Extension])
discoverExtensions =
  Unsafe.unsafePerformIO
    . Exception.try
    . discoverExtensionsIO

discoverExtensionsIO ::
  StringBuffer.StringBuffer ->
  IO (Maybe Flags.Language, [DynFlags.OnOff Extension.Extension])
discoverExtensionsIO stringBuffer = do
  (dynFlags, _, _) <- Session.parseDynamicFilePragma DynFlags.empty $ discoverOptions stringBuffer
  pure (DynFlags.language dynFlags, DynFlags.extensions dynFlags)

discoverOptions :: StringBuffer.StringBuffer -> [SrcLoc.Located String]
discoverOptions stringBuffer =
  snd $ Header.getOptions ParserOpts.empty stringBuffer interactiveFilePath

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
