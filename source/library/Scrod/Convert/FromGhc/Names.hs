-- | Name and signature extraction from GHC AST nodes.
--
-- Provides functions to extract declaration names, type signatures,
-- and related metadata from the various GHC declaration types. Used
-- by the main conversion module and by 'Scrod.Convert.FromGhc.Constructors'.
module Scrod.Convert.FromGhc.Names where

import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import GHC.Hs ()
import qualified GHC.Hs.Doc as HsDoc
import qualified GHC.Hs.Extension as Ghc
import qualified GHC.Types.SrcLoc as SrcLoc
import qualified GHC.Utils.Outputable as Outputable
import qualified Language.Haskell.Syntax as Syntax
import qualified Scrod.Convert.FromGhc.Internal as Internal
import qualified Scrod.Core.ItemName as ItemName

-- | Extract declaration name.
extractDeclName :: Syntax.LHsDecl Ghc.GhcPs -> Maybe ItemName.ItemName
extractDeclName lDecl = case SrcLoc.unLoc lDecl of
  Syntax.TyClD _ tyClDecl -> extractTyClDeclName tyClDecl
  Syntax.ValD _ bind -> extractBindName bind
  Syntax.SigD _ sig -> extractSigName sig
  Syntax.InstD _ inst -> extractInstDeclName inst
  Syntax.DerivD _ derivDecl -> extractDerivDeclName derivDecl
  Syntax.KindSigD _ kindSig -> Just $ extractStandaloneKindSigName kindSig
  Syntax.ForD _ foreignDecl -> Just $ extractForeignDeclName foreignDecl
  _ -> Nothing

-- | Extract name from a standalone kind signature.
extractStandaloneKindSigName :: Syntax.StandaloneKindSig Ghc.GhcPs -> ItemName.ItemName
extractStandaloneKindSigName (Syntax.StandaloneKindSig _ lName _) = Internal.extractIdPName lName

-- | Extract signature from a standalone kind signature.
-- Only returns the kind type, not the name.
-- For example, @type X :: a -> a@ produces @"a -> a"@.
extractKindSigSignature :: Syntax.StandaloneKindSig Ghc.GhcPs -> Text.Text
extractKindSigSignature (Syntax.StandaloneKindSig _ _ lSigType) =
  Text.pack . Outputable.showSDocUnsafe . Outputable.ppr $ lSigType

-- | Extract name from a type/class declaration.
extractTyClDeclName :: Syntax.TyClDecl Ghc.GhcPs -> Maybe ItemName.ItemName
extractTyClDeclName tyClDecl = case tyClDecl of
  Syntax.FamDecl _ famDecl -> Just $ extractFamilyDeclName famDecl
  Syntax.SynDecl {Syntax.tcdLName = lName} -> Just $ Internal.extractIdPName lName
  Syntax.DataDecl {Syntax.tcdLName = lName} -> Just $ Internal.extractIdPName lName
  Syntax.ClassDecl {Syntax.tcdLName = lName} -> Just $ Internal.extractIdPName lName

-- | Extract the fully applied parent type text from a data declaration.
-- For @data Maybe a@, this produces @"Maybe a"@.
extractParentTypeText :: Syntax.TyClDecl Ghc.GhcPs -> Maybe Text.Text
extractParentTypeText tyClDecl = case tyClDecl of
  Syntax.DataDecl {Syntax.tcdLName = lName, Syntax.tcdTyVars = tyVars} ->
    Just . Text.pack . Outputable.showSDocUnsafe $ case Syntax.hsQTvExplicit tyVars of
      [] -> Outputable.ppr lName
      tvs -> Outputable.ppr lName Outputable.<+> Outputable.hsep (fmap Outputable.ppr tvs)
  _ -> Nothing

-- | Extract type variable bindings from a type\/class declaration.
-- For @data T a b@, this produces @Just "a b"@.
-- For @class C a@, this produces @Just "a"@.
-- Returns 'Nothing' if there are no type variables.
extractTyClDeclTyVars :: Syntax.TyClDecl Ghc.GhcPs -> Maybe Text.Text
extractTyClDeclTyVars tyClDecl = case tyClDecl of
  Syntax.DataDecl {Syntax.tcdTyVars = tyVars} -> tyVarsToText tyVars
  Syntax.ClassDecl {Syntax.tcdTyVars = tyVars} -> tyVarsToText tyVars
  _ -> Nothing

-- | Pretty-print explicit type variable binders as text.
-- Returns 'Nothing' if the list is empty.
tyVarsToText :: Syntax.LHsQTyVars Ghc.GhcPs -> Maybe Text.Text
tyVarsToText tyVars = case Syntax.hsQTvExplicit tyVars of
  [] -> Nothing
  tvs -> Just . Text.pack . Outputable.showSDocUnsafe $ Outputable.hsep (fmap Outputable.ppr tvs)

-- | Extract the signature for a type synonym declaration.
-- For @type T = ()@, this produces @Just "= ()"@.
-- For @type T a = [a]@, this produces @Just "a = [a]"@.
extractSynDeclSignature :: Syntax.TyClDecl Ghc.GhcPs -> Maybe Text.Text
extractSynDeclSignature tyClDecl = case tyClDecl of
  Syntax.SynDecl {Syntax.tcdTyVars = tyVars, Syntax.tcdRhs = rhs} ->
    let rhsText = Text.pack . Outputable.showSDocUnsafe $ Outputable.ppr rhs
     in Just $ case tyVarsToText tyVars of
          Nothing -> Text.pack "= " <> rhsText
          Just tvs -> tvs <> Text.pack " = " <> rhsText
  _ -> Nothing

-- | Extract name from a family declaration.
extractFamilyDeclName :: Syntax.FamilyDecl Ghc.GhcPs -> ItemName.ItemName
extractFamilyDeclName famDecl = Internal.extractIdPName $ Syntax.fdLName famDecl

-- | Extract name from a foreign declaration.
extractForeignDeclName :: Syntax.ForeignDecl Ghc.GhcPs -> ItemName.ItemName
extractForeignDeclName foreignDecl = Internal.extractIdPName $ Syntax.fd_name foreignDecl

-- | Extract signature from a foreign declaration.
extractForeignDeclSignature :: Syntax.ForeignDecl Ghc.GhcPs -> Text.Text
extractForeignDeclSignature foreignDecl =
  Text.pack . Outputable.showSDocUnsafe . Outputable.ppr $ Syntax.fd_sig_ty foreignDecl

-- | Extract name from a binding.
extractBindName :: Syntax.HsBindLR Ghc.GhcPs Ghc.GhcPs -> Maybe ItemName.ItemName
extractBindName bind = case bind of
  Syntax.FunBind {Syntax.fun_id = lId} -> Just $ Internal.extractIdPName lId
  Syntax.PatBind {} -> Nothing
  Syntax.VarBind {} -> Nothing
  Syntax.PatSynBind _ patSyn -> Just $ extractPatSynName patSyn

-- | Extract name from a pattern synonym binding.
extractPatSynName :: Syntax.PatSynBind Ghc.GhcPs Ghc.GhcPs -> ItemName.ItemName
extractPatSynName patSyn = Internal.extractIdPName $ Syntax.psb_id patSyn

-- | Extract argument names from a function binding's patterns.
--
-- For each argument position, scans across all equations and picks
-- the first variable pattern name found (skipping wildcards,
-- constructor patterns, and literals). Returns one entry per
-- argument position; 'Nothing' when no variable name was found.
extractBindArgNames :: Syntax.HsBindLR Ghc.GhcPs Ghc.GhcPs -> [Maybe Text.Text]
extractBindArgNames bind = case bind of
  Syntax.FunBind {Syntax.fun_matches = mg} ->
    let lMatches = SrcLoc.unLoc (Syntax.mg_alts mg)
        patNameLists = fmap extractMatchPatNames lMatches
     in mergePatNames patNameLists
  _ -> []

-- | Extract the variable name (if any) from each pattern in a match.
extractMatchPatNames ::
  SrcLoc.GenLocated l (Syntax.Match Ghc.GhcPs body) ->
  [Maybe Text.Text]
extractMatchPatNames lMatch =
  let match = SrcLoc.unLoc lMatch
   in fmap (extractPatVarName . SrcLoc.unLoc) (SrcLoc.unLoc (Syntax.m_pats match))

-- | Extract a variable name from a pattern, unwrapping wrapper nodes.
--
-- Handles 'VarPat' directly, and recurses through 'AsPat' (using the
-- as-binding name), 'BangPat', 'LazyPat', 'ParPat', and 'SigPat'.
-- Returns 'Nothing' for wildcards, constructor patterns, literals, etc.
extractPatVarName :: Syntax.Pat Ghc.GhcPs -> Maybe Text.Text
extractPatVarName pat = case pat of
  Syntax.VarPat _ lId -> Just $ Internal.extractRdrName lId
  Syntax.AsPat _ lId _ -> Just $ Internal.extractRdrName lId
  Syntax.BangPat _ lPat -> extractPatVarName $ SrcLoc.unLoc lPat
  Syntax.LazyPat _ lPat -> extractPatVarName $ SrcLoc.unLoc lPat
  Syntax.ParPat _ lPat -> extractPatVarName $ SrcLoc.unLoc lPat
  _ -> Nothing

-- | Merge pattern names across multiple equations.
--
-- For each argument position, takes the first 'Just' name found
-- across the equations. This handles cases like:
--
-- @
-- or True _ = True
-- or _ x = x
-- @
--
-- where position 0 yields 'Nothing' (no variable in either equation)
-- and position 1 yields @Just "x"@ (from the second equation).
mergePatNames :: [[Maybe Text.Text]] -> [Maybe Text.Text]
mergePatNames [] = []
mergePatNames patNameLists =
  let maxLen = maximum (fmap length patNameLists)
      padded = fmap (\ns -> ns <> replicate (maxLen - length ns) Nothing) patNameLists
   in fmap (Maybe.listToMaybe . Maybe.catMaybes) (List.transpose padded)

-- | Extract name from a signature.
extractSigName :: Syntax.Sig Ghc.GhcPs -> Maybe ItemName.ItemName
extractSigName sig = case sig of
  Syntax.TypeSig _ (lName : _) _ -> Just $ Internal.extractIdPName lName
  Syntax.PatSynSig _ (lName : _) _ -> Just $ Internal.extractIdPName lName
  Syntax.ClassOpSig _ _ (lName : _) _ -> Just $ Internal.extractIdPName lName
  _ -> Nothing

-- | Extract signature text from a Sig. Only returns the type part, not the
-- name. For example, @x :: Int@ produces @"Int"@.
-- Strips 'HsDocTy' nodes so that embedded doc comments do not appear in the
-- pretty-printed output.
extractSigSignature :: Syntax.Sig Ghc.GhcPs -> Maybe Text.Text
extractSigSignature sig = case sig of
  Syntax.TypeSig _ _ ty ->
    Just . Text.pack . Outputable.showSDocUnsafe . Outputable.ppr $ stripHsSigWcType ty
  Syntax.PatSynSig _ _ ty ->
    Just . Text.pack . Outputable.showSDocUnsafe . Outputable.ppr $ stripHsSigType ty
  Syntax.ClassOpSig _ _ _ ty ->
    Just . Text.pack . Outputable.showSDocUnsafe . Outputable.ppr $ stripHsSigType ty
  _ -> Nothing

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

-- | Extract argument types and their optional doc comments from a type
-- signature. Walks the 'HsFunTy' chain, collecting each argument's
-- pretty-printed type text and its 'LHsDoc' (if the argument was wrapped
-- in 'HsDocTy'). The return type (final non-arrow part) is not included.
--
-- Handles 'TypeSig' (unwrap via 'hswc_body'), 'PatSynSig', and
-- 'ClassOpSig' (unwrap via 'sig_body' on 'HsSigType').
extractSigArguments ::
  Syntax.Sig Ghc.GhcPs -> [(Text.Text, Maybe (HsDoc.LHsDoc Ghc.GhcPs))]
extractSigArguments sig = case sig of
  Syntax.TypeSig _ _ wc ->
    extractArgsFromBody . Syntax.sig_body . SrcLoc.unLoc $ Syntax.hswc_body wc
  Syntax.PatSynSig _ _ lSigType ->
    extractArgsFromBody . Syntax.sig_body $ SrcLoc.unLoc lSigType
  Syntax.ClassOpSig _ _ _ lSigType ->
    extractArgsFromBody . Syntax.sig_body $ SrcLoc.unLoc lSigType
  _ -> []

-- | Skip through 'HsForAllTy' and 'HsQualTy' to reach the arrow chain,
-- then extract arguments from the 'HsFunTy' chain.
extractArgsFromBody ::
  Syntax.LHsType Ghc.GhcPs -> [(Text.Text, Maybe (HsDoc.LHsDoc Ghc.GhcPs))]
extractArgsFromBody lTy = case SrcLoc.unLoc lTy of
  Syntax.HsForAllTy _ _ body -> extractArgsFromBody body
  Syntax.HsQualTy _ _ body -> extractArgsFromBody body
  Syntax.HsFunTy _ _ arg res -> extractArg arg : extractArgsFromBody res
  Syntax.HsDocTy _ inner _doc -> case SrcLoc.unLoc inner of
    Syntax.HsFunTy _ _ arg res -> extractArg arg : extractArgsFromBody res
    _ -> extractArgsFromBody inner
  _ -> []

-- | Extract the type text and optional doc comment from a single argument.
-- If the argument is wrapped in 'HsDocTy', the doc is extracted and the
-- inner type is pretty-printed (with any nested doc annotations stripped).
-- Otherwise, the argument is pretty-printed as-is with no doc.
extractArg ::
  Syntax.LHsType Ghc.GhcPs -> (Text.Text, Maybe (HsDoc.LHsDoc Ghc.GhcPs))
extractArg lTy = case SrcLoc.unLoc lTy of
  Syntax.HsDocTy _ inner doc ->
    ( Text.pack . Outputable.showSDocUnsafe . Outputable.ppr $ stripHsDocTy inner,
      Just doc
    )
  _ ->
    ( Text.pack . Outputable.showSDocUnsafe . Outputable.ppr $ stripHsDocTy lTy,
      Nothing
    )

-- | Extract name from an instance declaration.
extractInstDeclName :: Syntax.InstDecl Ghc.GhcPs -> Maybe ItemName.ItemName
extractInstDeclName inst = Just $ case inst of
  Syntax.ClsInstD _ clsInst ->
    ItemName.MkItemName . Text.pack . Outputable.showSDocUnsafe . Outputable.ppr $
      Syntax.cid_poly_ty clsInst
  Syntax.DataFamInstD _ dataFamInst ->
    ItemName.MkItemName . Text.pack . Outputable.showSDocUnsafe . Outputable.ppr $
      dataFamInst
  Syntax.TyFamInstD _ tyFamInst ->
    ItemName.MkItemName . Text.pack . Outputable.showSDocUnsafe . Outputable.ppr $
      tyFamInst

-- | Extract name from a standalone deriving declaration.
extractDerivDeclName :: Syntax.DerivDecl Ghc.GhcPs -> Maybe ItemName.ItemName
extractDerivDeclName =
  Just
    . ItemName.MkItemName
    . Text.pack
    . Outputable.showSDocUnsafe
    . Outputable.ppr
    . Syntax.hswc_body
    . Syntax.deriv_type

-- | Extract names from a constructor declaration.
-- GADT constructors can declare multiple names (e.g. @A, B :: Int -> T@).
extractConDeclNames :: Syntax.ConDecl Ghc.GhcPs -> NonEmpty.NonEmpty ItemName.ItemName
extractConDeclNames conDecl = case conDecl of
  Syntax.ConDeclH98 {Syntax.con_name = lName} -> pure $ Internal.extractIdPName lName
  Syntax.ConDeclGADT {Syntax.con_names = lNames} ->
    fmap Internal.extractIdPName lNames
