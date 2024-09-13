{-# LANGUAGE FlexibleInstances #-}

module Scrod where

import qualified Data.Data as Data
import qualified GHC.Data.EnumSet as EnumSet
import qualified GHC.Data.FastString as FastString
import qualified GHC.Data.StringBuffer as StringBuffer
import qualified GHC.Hs
import qualified GHC.Parser as Parser
import qualified GHC.Parser.Errors.Types as PsErr
import qualified GHC.Parser.Lexer as Lexer
import qualified GHC.Types.SrcLoc as SrcLoc
import qualified GHC.Utils.Error as ErrUtil
import qualified GHC.Utils.Outputable as Outputable
import qualified Language.Haskell.Syntax as HS

parseLHsModule ::
  -- | Included in source spans but really only needed for error messages.
  FilePath ->
  -- | The source code to parse. Not currently clear which encoding Haskell
  -- files are expected to use. Probably UTF-8?
  String ->
  Either (Messages PsErr.PsMessage) (LHsModule GHC.Hs.GhcPs)
parseLHsModule filePath string =
  let parserOpts =
        Lexer.mkParserOpts
          EnumSet.empty -- enabled extensions
          ErrUtil.emptyDiagOpts -- diagnostic options
          [] -- supported extensions
          False -- enable safe imports?
          True -- keep Haddock comments?
          True -- keep regular comments?
          False -- allow position to be updated by pragmas?
      realSrcLoc =
        SrcLoc.mkRealSrcLoc
          (FastString.mkFastString filePath)
          1 -- first row
          1 -- first column
      pState =
        Lexer.initParserState
          parserOpts
          (StringBuffer.stringToStringBuffer string)
          realSrcLoc
   in case Lexer.unP Parser.parseModule pState of
        Lexer.PFailed newPState -> Left . Messages $ Lexer.getPsErrorMessages newPState
        Lexer.POk _warnings lHsModule -> Right $ LHsModule lHsModule

-- | Wrapper to avoid orphans.
newtype LHsModule a = LHsModule
  { unwrapLHsModule :: SrcLoc.Located (HS.HsModule a)
  }

-- | Provided for convenience in GHCi.
instance Show (LHsModule GHC.Hs.GhcPs) where
  show = ($ "") . gshows . unwrapLHsModule

-- | Wrapper to avoid orphans.
newtype Messages a = Messages
  { unwrapMessages :: ErrUtil.Messages a
  }

-- | Provided for convenience in GHCi.
instance (ErrUtil.Diagnostic a) => Show (Messages a) where
  show = Outputable.showPprUnsafe . unwrapMessages

-- | Taken from <https://hackage.haskell.org/package/syb-0.7.2.4>.
gshows :: (Data.Data a) => a -> ShowS
gshows =
  let extQ ::
        (Data.Typeable a, Data.Typeable b) =>
        (a -> r) -> (b -> r) -> a -> r
      extQ f g a = maybe (f a) g (Data.cast a)
   in ( \t ->
          showChar '('
            . showString (Data.showConstr $ Data.toConstr t)
            . foldr (.) id (Data.gmapQ ((showChar ' ' .) . gshows) t)
            . showChar ')'
      )
        `extQ` (shows :: String -> ShowS)
