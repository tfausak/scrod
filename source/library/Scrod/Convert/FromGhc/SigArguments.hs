-- | Argument and return-type extraction from GHC type signatures.
--
-- Provides functions to walk the 'HsFunTy' chain of a type signature,
-- collecting each argument's pretty-printed type text and its optional
-- Haddock documentation. Also provides utilities to strip 'HsDocTy'
-- wrappers from types before pretty-printing.
module Scrod.Convert.FromGhc.SigArguments where

import qualified Data.Text as Text
import GHC.Hs ()
import qualified GHC.Hs.Doc as HsDoc
import qualified GHC.Hs.Extension as Ghc
import qualified GHC.Types.SrcLoc as SrcLoc
import qualified GHC.Utils.Outputable as Outputable
import qualified Language.Haskell.Syntax as Syntax
import qualified Scrod.Convert.FromGhc.Internal as Internal

-- | Extract argument types and their optional doc comments from a type
-- signature. Walks the 'HsFunTy' chain, collecting each argument's
-- pretty-printed type text and its 'LHsDoc' (if the argument was wrapped
-- in 'HsDocTy'). The return type is included only when it has documentation.
--
-- Returns a pair of (arguments, optional return type).
--
-- Handles 'TypeSig' (unwrap via 'hswc_body'), 'PatSynSig', and
-- 'ClassOpSig' (unwrap via 'sig_body' on 'HsSigType').
extractSigArguments ::
  Syntax.Sig Ghc.GhcPs ->
  ( [(Text.Text, Maybe (HsDoc.LHsDoc Ghc.GhcPs))],
    Maybe (Text.Text, Maybe (HsDoc.LHsDoc Ghc.GhcPs))
  )
extractSigArguments sig = case sig of
  Syntax.TypeSig _ _ wc ->
    extractArgsFromBody . Syntax.sig_body . SrcLoc.unLoc $ Syntax.hswc_body wc
  Syntax.PatSynSig _ _ lSigType ->
    extractArgsFromBody . Syntax.sig_body $ SrcLoc.unLoc lSigType
  Syntax.ClassOpSig _ _ _ lSigType ->
    extractArgsFromBody . Syntax.sig_body $ SrcLoc.unLoc lSigType
  _ -> ([], Nothing)

-- | Skip through 'HsForAllTy' and 'HsQualTy' to reach the arrow chain,
-- then extract arguments from the 'HsFunTy' chain. Returns the arguments
-- and the optional documented return type separately.
extractArgsFromBody ::
  Syntax.LHsType Ghc.GhcPs ->
  ( [(Text.Text, Maybe (HsDoc.LHsDoc Ghc.GhcPs))],
    Maybe (Text.Text, Maybe (HsDoc.LHsDoc Ghc.GhcPs))
  )
extractArgsFromBody lTy = case SrcLoc.unLoc lTy of
  Syntax.HsForAllTy _ _ body -> extractArgsFromBody body
  Syntax.HsQualTy _ _ body -> extractArgsFromBody body
  Syntax.HsFunTy _ _ arg res ->
    let (args, ret) = extractArgsFromBody res
     in (extractArg arg : args, ret)
  Syntax.HsDocTy _ inner _doc -> case SrcLoc.unLoc inner of
    Syntax.HsFunTy _ _ arg res ->
      let (args, ret) = extractArgsFromBody res
       in (extractArg arg : args, ret)
    _ -> ([], Just (extractArg lTy))
  _ -> ([], Nothing)

-- | Extract the type text and optional doc comment from a single argument.
-- If the argument is wrapped in 'HsDocTy', the doc is extracted and the
-- inner type is pretty-printed (with any nested doc annotations stripped).
-- Otherwise, the argument is pretty-printed as-is with no doc.
extractArg ::
  Syntax.LHsType Ghc.GhcPs -> (Text.Text, Maybe (HsDoc.LHsDoc Ghc.GhcPs))
extractArg lTy = case SrcLoc.unLoc lTy of
  Syntax.HsDocTy _ inner doc ->
    ( Text.pack . Internal.showSDocShort . Outputable.ppr $ stripHsDocTy inner,
      Just doc
    )
  _ ->
    ( Text.pack . Internal.showSDocShort . Outputable.ppr $ stripHsDocTy lTy,
      Nothing
    )

-- | Strip 'HsDocTy' wrappers from a wildcard-wrapped signature type.
stripHsSigWcType ::
  Syntax.LHsSigWcType Ghc.GhcPs -> Syntax.LHsSigWcType Ghc.GhcPs
stripHsSigWcType wc =
  wc {Syntax.hswc_body = stripHsSigType (Syntax.hswc_body wc)}

-- | Strip 'HsDocTy' wrappers from a signature type.
stripHsSigType :: Syntax.LHsSigType Ghc.GhcPs -> Syntax.LHsSigType Ghc.GhcPs
stripHsSigType lSigType = case SrcLoc.unLoc lSigType of
  Syntax.HsSig {Syntax.sig_ext = ext, Syntax.sig_bndrs = bndrs, Syntax.sig_body = body} ->
    let stripped = stripHsDocTy body
     in SrcLoc.L
          (SrcLoc.getLoc lSigType)
          Syntax.HsSig
            { Syntax.sig_ext = ext,
              Syntax.sig_bndrs = bndrs,
              Syntax.sig_body = stripped
            }

-- | Recursively strip 'HsDocTy' wrappers from a located type.
-- Recurses into 'HsFunTy', 'HsForAllTy', 'HsQualTy', and 'HsParTy' to
-- ensure all embedded doc comments are removed.
stripHsDocTy :: Syntax.LHsType Ghc.GhcPs -> Syntax.LHsType Ghc.GhcPs
stripHsDocTy lTy = case lTy of
  SrcLoc.L ann hsType -> case hsType of
    Syntax.HsDocTy _ inner _ -> stripHsDocTy inner
    Syntax.HsFunTy x mult arg res ->
      SrcLoc.L ann $ Syntax.HsFunTy x mult (stripHsDocTy arg) (stripHsDocTy res)
    Syntax.HsForAllTy x tele body ->
      SrcLoc.L ann $ Syntax.HsForAllTy x tele (stripHsDocTy body)
    Syntax.HsQualTy x ctx body ->
      SrcLoc.L ann $ Syntax.HsQualTy x ctx (stripHsDocTy body)
    Syntax.HsParTy x inner ->
      SrcLoc.L ann $ Syntax.HsParTy x (stripHsDocTy inner)
    _ -> lTy
