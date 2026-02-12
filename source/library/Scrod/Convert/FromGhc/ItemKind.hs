-- | Map GHC declaration types to Scrod's 'ItemKind.ItemKind'.
--
-- Pure functions that inspect GHC AST nodes and return the
-- corresponding 'ItemKind' value. No monadic state or internal
-- dependencies.
module Scrod.Convert.FromGhc.ItemKind where

import GHC.Hs ()
import qualified GHC.Hs.Extension as Ghc
import qualified Language.Haskell.Syntax as Syntax
import qualified Scrod.Core.ItemKind as ItemKind

-- | Determine the ItemKind from a declaration.
itemKindFromDecl :: Syntax.HsDecl Ghc.GhcPs -> ItemKind.ItemKind
itemKindFromDecl decl = case decl of
  Syntax.TyClD _ tyClDecl -> itemKindFromTyClDecl tyClDecl
  Syntax.ValD _ bind -> itemKindFromBind bind
  Syntax.SigD _ sig -> itemKindFromSig sig
  Syntax.InstD _ inst -> itemKindFromInstDecl inst
  Syntax.KindSigD {} -> ItemKind.StandaloneKindSig
  Syntax.DefD {} -> ItemKind.Default
  Syntax.ForD _ foreignDecl -> itemKindFromForeignDecl foreignDecl
  Syntax.WarningD {} -> ItemKind.Function -- Treat as function for now
  Syntax.AnnD {} -> ItemKind.Annotation
  Syntax.RuleD {} -> ItemKind.Rule
  Syntax.SpliceD {} -> ItemKind.Splice
  Syntax.DocD {} -> ItemKind.Function -- Doc comment
  Syntax.RoleAnnotD {} -> ItemKind.Function -- Role annotation
  Syntax.DerivD {} -> ItemKind.StandaloneDeriving

-- | Determine ItemKind from a type/class declaration.
itemKindFromTyClDecl :: Syntax.TyClDecl Ghc.GhcPs -> ItemKind.ItemKind
itemKindFromTyClDecl tyClDecl = case tyClDecl of
  Syntax.FamDecl _ famDecl -> itemKindFromFamilyDecl famDecl
  Syntax.SynDecl {} -> ItemKind.TypeSynonym
  Syntax.DataDecl _ _ _ _ dataDefn -> itemKindFromDataDefn dataDefn
  Syntax.ClassDecl {} -> ItemKind.Class

-- | Determine ItemKind from a data definition.
itemKindFromDataDefn :: Syntax.HsDataDefn Ghc.GhcPs -> ItemKind.ItemKind
itemKindFromDataDefn dataDefn = case Syntax.dd_cons dataDefn of
  Syntax.NewTypeCon {} -> ItemKind.Newtype
  Syntax.DataTypeCons isTypeData _ ->
    if isTypeData
      then ItemKind.TypeData
      else ItemKind.DataType

-- | Determine ItemKind from a family declaration.
itemKindFromFamilyDecl :: Syntax.FamilyDecl Ghc.GhcPs -> ItemKind.ItemKind
itemKindFromFamilyDecl famDecl = case Syntax.fdInfo famDecl of
  Syntax.DataFamily -> ItemKind.DataFamily
  Syntax.OpenTypeFamily -> ItemKind.OpenTypeFamily
  Syntax.ClosedTypeFamily {} -> ItemKind.ClosedTypeFamily

-- | Determine ItemKind from a binding.
itemKindFromBind :: Syntax.HsBindLR Ghc.GhcPs Ghc.GhcPs -> ItemKind.ItemKind
itemKindFromBind bind = case bind of
  Syntax.FunBind {} -> ItemKind.Function
  Syntax.PatBind {} -> ItemKind.PatternBinding
  Syntax.VarBind {} -> ItemKind.Function
  Syntax.PatSynBind {} -> ItemKind.PatternSynonym

-- | Determine ItemKind from a signature.
itemKindFromSig :: Syntax.Sig Ghc.GhcPs -> ItemKind.ItemKind
itemKindFromSig sig = case sig of
  Syntax.TypeSig {} -> ItemKind.Function
  Syntax.PatSynSig {} -> ItemKind.PatternSynonym
  Syntax.ClassOpSig {} -> ItemKind.ClassMethod
  Syntax.FixSig {} -> ItemKind.FixitySignature
  Syntax.InlineSig {} -> ItemKind.InlineSignature
  Syntax.SpecSig {} -> ItemKind.SpecialiseSignature
  _ -> ItemKind.Function

-- | Determine ItemKind from an instance declaration.
itemKindFromInstDecl :: Syntax.InstDecl Ghc.GhcPs -> ItemKind.ItemKind
itemKindFromInstDecl inst = case inst of
  Syntax.ClsInstD {} -> ItemKind.ClassInstance
  Syntax.DataFamInstD {} -> ItemKind.DataFamilyInstance
  Syntax.TyFamInstD {} -> ItemKind.TypeFamilyInstance

-- | Determine ItemKind from a foreign declaration.
itemKindFromForeignDecl :: Syntax.ForeignDecl Ghc.GhcPs -> ItemKind.ItemKind
itemKindFromForeignDecl foreignDecl = case foreignDecl of
  Syntax.ForeignImport {} -> ItemKind.ForeignImport
  Syntax.ForeignExport {} -> ItemKind.ForeignExport
