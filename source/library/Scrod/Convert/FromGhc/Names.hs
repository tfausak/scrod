-- | Name and signature extraction from GHC AST nodes.
--
-- Provides functions to extract declaration names, type signatures,
-- and related metadata from the various GHC declaration types. Used
-- by the main conversion module and by 'Scrod.Convert.FromGhc.Constructors'.
module Scrod.Convert.FromGhc.Names where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import GHC.Hs ()
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

-- | Extract signature from a kind signature.
extractKindSigSignature :: Syntax.StandaloneKindSig Ghc.GhcPs -> Text.Text
extractKindSigSignature = Text.pack . Outputable.showSDocUnsafe . Outputable.ppr

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

-- | Extract name from a signature.
extractSigName :: Syntax.Sig Ghc.GhcPs -> Maybe ItemName.ItemName
extractSigName sig = case sig of
  Syntax.TypeSig _ (lName : _) _ -> Just $ Internal.extractIdPName lName
  Syntax.PatSynSig _ (lName : _) _ -> Just $ Internal.extractIdPName lName
  Syntax.ClassOpSig _ _ (lName : _) _ -> Just $ Internal.extractIdPName lName
  _ -> Nothing

-- | Extract signature text from a Sig. Only returns the type part, not the
-- name. For example, @x :: Int@ produces @"Int"@.
extractSigSignature :: Syntax.Sig Ghc.GhcPs -> Maybe Text.Text
extractSigSignature sig = case sig of
  Syntax.TypeSig _ _ ty -> Just . Text.pack . Outputable.showSDocUnsafe $ Outputable.ppr ty
  Syntax.PatSynSig _ _ ty -> Just . Text.pack . Outputable.showSDocUnsafe $ Outputable.ppr ty
  Syntax.ClassOpSig _ _ _ ty -> Just . Text.pack . Outputable.showSDocUnsafe $ Outputable.ppr ty
  _ -> Nothing

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
