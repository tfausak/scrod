{-# LANGUAGE OverloadedStrings #-}

module Scrod.Extra.Signature where

import qualified Data.Text as Text
import qualified GHC.Hs as Hs
import qualified GHC.Hs.Extension as Ghc
import qualified GHC.Types.SrcLoc as SrcLoc
import qualified GHC.Utils.Outputable as Outputable
import qualified Language.Haskell.Syntax as Syntax

-- | Convert any Outputable type to signature text.
toSignatureText :: (Outputable.Outputable a) => a -> Text.Text
toSignatureText =
  Text.pack . Outputable.showSDocUnsafe . Outputable.ppr

-- | Extract signature from a Sig declaration.
-- Uses a simple approach: render the whole signature and extract the type part.
extractFromSig :: Syntax.Sig Ghc.GhcPs -> Maybe Text.Text
extractFromSig sig = case sig of
  Syntax.TypeSig _ _ sigType ->
    Just $ extractTypeFromWildCards sigType
  Syntax.PatSynSig _ _ sigType ->
    Just $ extractTypeFromSigType sigType
  Syntax.ClassOpSig _ _ _ sigType ->
    Just $ extractTypeFromSigType sigType
  _ -> Nothing

-- | Extract type from HsWildCardBndrs wrapper.
extractTypeFromWildCards :: Hs.LHsSigWcType Ghc.GhcPs -> Text.Text
extractTypeFromWildCards wcType =
  let sigType = Hs.hswc_body wcType
   in extractTypeFromSigType sigType

-- | Extract type from HsSigType.
extractTypeFromSigType :: Hs.LHsSigType Ghc.GhcPs -> Text.Text
extractTypeFromSigType lSigType =
  let sigType = SrcLoc.unLoc lSigType
      bodyType = Syntax.sig_body sigType
   in toSignatureText bodyType

-- | Extract signature from a standalone kind signature.
extractFromKindSig :: Syntax.StandaloneKindSig Ghc.GhcPs -> Text.Text
extractFromKindSig (Syntax.StandaloneKindSig _ _ lHsSigType) =
  extractTypeFromSigType lHsSigType

-- | Extract signature from a constructor declaration.
extractFromConDecl :: Syntax.ConDecl Ghc.GhcPs -> Maybe Text.Text
extractFromConDecl conDecl = case conDecl of
  Syntax.ConDeclGADT {Syntax.con_g_args = args, Syntax.con_res_ty = resTy} ->
    -- For GADT constructors, combine arguments and result type
    let argsText = case args of
          Hs.PrefixConGADT _ argTypes -> Text.intercalate " -> " (fmap (toSignatureText . Hs.hsScaledThing) argTypes)
          Hs.RecConGADT _ _ -> ""
        resText = toSignatureText resTy
     in if Text.null argsText
          then Just resText
          else Just $ argsText <> " -> " <> resText
  Syntax.ConDeclH98 {Syntax.con_args = args} ->
    extractFromH98Args args

-- | Extract signature from H98-style constructor arguments.
extractFromH98Args :: Syntax.HsConDeclH98Details Ghc.GhcPs -> Maybe Text.Text
extractFromH98Args details = case details of
  Syntax.PrefixCon _ args ->
    if null args
      then Nothing
      else Just $ Text.intercalate " -> " (fmap (toSignatureText . Hs.hsScaledThing) args)
  Syntax.InfixCon arg1 arg2 ->
    Just $ toSignatureText (Hs.hsScaledThing arg1) <> " -> " <> toSignatureText (Hs.hsScaledThing arg2)
  Syntax.RecCon _ -> Nothing

-- | Extract signature from a record field.
extractFromConDeclField :: Syntax.ConDeclField Ghc.GhcPs -> Text.Text
extractFromConDeclField field =
  toSignatureText $ Hs.cd_fld_type field
