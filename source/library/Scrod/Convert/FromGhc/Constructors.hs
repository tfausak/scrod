-- | Convert constructor declarations and record fields.
--
-- Handles both H98-style and GADT constructor declarations, including
-- signature extraction, documentation stripping, and record field
-- conversion.
module Scrod.Convert.FromGhc.Constructors where

import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified GHC.Hs.Extension as Ghc
import qualified GHC.Parser.Annotation as Annotation
import qualified GHC.Types.SrcLoc as SrcLoc
import qualified GHC.Utils.Outputable as Outputable
import qualified Language.Haskell.Syntax as Syntax
import qualified Scrod.Convert.FromGhc.Doc as GhcDoc
import qualified Scrod.Convert.FromGhc.Internal as Internal
import qualified Scrod.Convert.FromGhc.Names as Names
import qualified Scrod.Core.Doc as Doc
import qualified Scrod.Core.Item as Item
import qualified Scrod.Core.ItemKey as ItemKey
import qualified Scrod.Core.ItemKind as ItemKind
import qualified Scrod.Core.Located as Located

-- | Convert a constructor declaration.
convertConDeclM ::
  Maybe ItemKey.ItemKey ->
  Maybe Text.Text ->
  Syntax.LConDecl Ghc.GhcPs ->
  Internal.ConvertM [Located.Located Item.Item]
convertConDeclM parentKey parentType lConDecl = do
  let conDecl = SrcLoc.unLoc lConDecl
      conDoc = extractConDeclDoc conDecl
      conSig = extractConDeclSignature parentType conDecl
      conKind = constructorKind conDecl
  result <-
    Internal.mkItemWithKeyM
      (Annotation.getLocA lConDecl)
      parentKey
      (Just $ Names.extractConDeclName conDecl)
      conDoc
      conSig
      conKind
  case result of
    Nothing -> pure []
    Just (constructorItem, key) -> do
      fieldItems <- extractFieldsFromConDeclM (Just key) conDecl
      pure $ [constructorItem] <> fieldItems

-- | Determine constructor kind.
constructorKind :: Syntax.ConDecl Ghc.GhcPs -> ItemKind.ItemKind
constructorKind conDecl = case conDecl of
  Syntax.ConDeclH98 {} -> ItemKind.DataConstructor
  Syntax.ConDeclGADT {} -> ItemKind.GADTConstructor

-- | Extract documentation from a constructor declaration.
extractConDeclDoc :: Syntax.ConDecl Ghc.GhcPs -> Doc.Doc
extractConDeclDoc conDecl = case conDecl of
  Syntax.ConDeclH98 {Syntax.con_doc = mDoc} ->
    maybe Doc.Empty GhcDoc.convertLHsDoc mDoc
  Syntax.ConDeclGADT {Syntax.con_doc = mDoc} ->
    maybe Doc.Empty GhcDoc.convertLHsDoc mDoc

-- | Extract signature from a constructor declaration.
-- Returns only the type portion (no constructor name or @::@).
extractConDeclSignature :: Maybe Text.Text -> Syntax.ConDecl Ghc.GhcPs -> Maybe Text.Text
extractConDeclSignature mParentType conDecl = case conDecl of
  Syntax.ConDeclH98
    { Syntax.con_forall = hasForall,
      Syntax.con_ex_tvs = exTvs,
      Syntax.con_mb_cxt = mbCxt,
      Syntax.con_args = args
    } ->
      case mParentType of
        Nothing ->
          Just . Text.pack . Outputable.showSDocUnsafe . Outputable.ppr $
            conDecl
              { Syntax.con_doc = Nothing,
                Syntax.con_args = stripH98DetailsDocs args
              }
        Just parentType ->
          let forallDoc =
                if hasForall && not (null exTvs)
                  then
                    Outputable.text "forall"
                      Outputable.<+> Outputable.hsep (fmap Outputable.ppr exTvs)
                      Outputable.<> Outputable.text "."
                  else Outputable.empty
              cxtDoc = case mbCxt of
                Nothing -> Outputable.empty
                Just ctx -> case SrcLoc.unLoc ctx of
                  [] -> Outputable.empty
                  [c] -> Outputable.ppr c Outputable.<+> Outputable.text "=>"
                  cs ->
                    Outputable.parens
                      (Outputable.hsep (Outputable.punctuate (Outputable.text ",") (fmap Outputable.ppr cs)))
                      Outputable.<+> Outputable.text "=>"
              argsDoc = h98ArgsToDoc (stripH98DetailsDocs args)
              bodyDoc = case argsDoc of
                Nothing -> Outputable.text (Text.unpack parentType)
                Just ad -> ad Outputable.<+> Outputable.text "->" Outputable.<+> Outputable.text (Text.unpack parentType)
           in Just . Text.pack . Outputable.showSDocUnsafe $
                forallDoc Outputable.<+> cxtDoc Outputable.<+> bodyDoc
  c@Syntax.ConDeclGADT {} ->
    let full =
          Text.pack . Outputable.showSDocUnsafe . Outputable.ppr $
            c
              { Syntax.con_doc = Nothing,
                Syntax.con_g_args = stripGADTDetailsDocs (Syntax.con_g_args c)
              }
        sep = Text.pack " :: "
        (_, rest) = Text.breakOn sep full
     in Just $ if Text.null rest then full else Text.drop (Text.length sep) rest

-- | Convert H98 constructor arguments to an arrow-separated SDoc.
h98ArgsToDoc ::
  Syntax.HsConDeclH98Details Ghc.GhcPs ->
  Maybe Outputable.SDoc
h98ArgsToDoc details = case details of
  Syntax.PrefixCon [] -> Nothing
  Syntax.PrefixCon fields ->
    Just
      . Outputable.hsep
      . List.intersperse (Outputable.text "->")
      $ fmap (Outputable.ppr . Syntax.cdf_type) fields
  Syntax.InfixCon l r ->
    Just $
      Outputable.ppr (Syntax.cdf_type l)
        Outputable.<+> Outputable.text "->"
        Outputable.<+> Outputable.ppr (Syntax.cdf_type r)
  Syntax.RecCon lFields ->
    Just $
      Outputable.text "{"
        Outputable.<+> Outputable.hsep
          (Outputable.punctuate (Outputable.text ",") (fmap Outputable.ppr (SrcLoc.unLoc lFields)))
        Outputable.<+> Outputable.text "}"

-- | Strip documentation from H98 constructor details.
stripH98DetailsDocs ::
  Syntax.HsConDeclH98Details Ghc.GhcPs ->
  Syntax.HsConDeclH98Details Ghc.GhcPs
stripH98DetailsDocs details = case details of
  Syntax.PrefixCon fields -> Syntax.PrefixCon (fmap stripFieldDoc fields)
  Syntax.InfixCon l r -> Syntax.InfixCon (stripFieldDoc l) (stripFieldDoc r)
  Syntax.RecCon lFields -> Syntax.RecCon (fmap (fmap (fmap stripRecFieldDoc)) lFields)

-- | Strip documentation from GADT constructor details.
stripGADTDetailsDocs ::
  Syntax.HsConDeclGADTDetails Ghc.GhcPs ->
  Syntax.HsConDeclGADTDetails Ghc.GhcPs
stripGADTDetailsDocs details = case details of
  Syntax.PrefixConGADT x fields -> Syntax.PrefixConGADT x (fmap stripFieldDoc fields)
  Syntax.RecConGADT x lFields -> Syntax.RecConGADT x (fmap (fmap (fmap stripRecFieldDoc)) lFields)

-- | Strip documentation from a constructor field.
stripFieldDoc :: Syntax.HsConDeclField Ghc.GhcPs -> Syntax.HsConDeclField Ghc.GhcPs
stripFieldDoc f@Syntax.CDF {} = f {Syntax.cdf_doc = Nothing}

-- | Strip documentation from a record field.
stripRecFieldDoc ::
  Syntax.HsConDeclRecField Ghc.GhcPs ->
  Syntax.HsConDeclRecField Ghc.GhcPs
stripRecFieldDoc f@Syntax.HsConDeclRecField {} =
  f {Syntax.cdrf_spec = stripFieldDoc (Syntax.cdrf_spec f)}

-- | Extract fields from a constructor declaration.
extractFieldsFromConDeclM ::
  Maybe ItemKey.ItemKey ->
  Syntax.ConDecl Ghc.GhcPs ->
  Internal.ConvertM [Located.Located Item.Item]
extractFieldsFromConDeclM parentKey conDecl = case conDecl of
  Syntax.ConDeclH98 {Syntax.con_args = args} ->
    extractFieldsFromH98DetailsM parentKey args
  Syntax.ConDeclGADT {Syntax.con_g_args = gArgs} ->
    extractFieldsFromGADTDetailsM parentKey gArgs

-- | Extract fields from H98-style constructor details.
extractFieldsFromH98DetailsM ::
  Maybe ItemKey.ItemKey ->
  Syntax.HsConDeclH98Details Ghc.GhcPs ->
  Internal.ConvertM [Located.Located Item.Item]
extractFieldsFromH98DetailsM parentKey details = case details of
  Syntax.PrefixCon _ -> pure []
  Syntax.InfixCon _ _ -> pure []
  Syntax.RecCon lFields -> convertConDeclFieldsM parentKey (SrcLoc.unLoc lFields)

-- | Extract fields from GADT-style constructor details.
extractFieldsFromGADTDetailsM ::
  Maybe ItemKey.ItemKey ->
  Syntax.HsConDeclGADTDetails Ghc.GhcPs ->
  Internal.ConvertM [Located.Located Item.Item]
extractFieldsFromGADTDetailsM parentKey details = case details of
  Syntax.PrefixConGADT _ _ -> pure []
  Syntax.RecConGADT _ lFields -> convertConDeclFieldsM parentKey (SrcLoc.unLoc lFields)

-- | Convert a list of record fields.
convertConDeclFieldsM ::
  Maybe ItemKey.ItemKey ->
  [Syntax.LHsConDeclRecField Ghc.GhcPs] ->
  Internal.ConvertM [Located.Located Item.Item]
convertConDeclFieldsM parentKey = fmap concat . traverse (convertConDeclFieldM parentKey)

-- | Convert a single record field declaration to items (one per field name).
convertConDeclFieldM ::
  Maybe ItemKey.ItemKey ->
  Syntax.LHsConDeclRecField Ghc.GhcPs ->
  Internal.ConvertM [Located.Located Item.Item]
convertConDeclFieldM parentKey lField =
  let recField = SrcLoc.unLoc lField
      fieldSpec = Syntax.cdrf_spec recField
      doc = maybe Doc.Empty GhcDoc.convertLHsDoc $ Syntax.cdf_doc fieldSpec
      sig = Just . Text.pack . Outputable.showSDocUnsafe . Outputable.ppr $ Syntax.cdf_type fieldSpec
   in Maybe.catMaybes <$> traverse (convertFieldNameM parentKey doc sig) (Syntax.cdrf_names recField)

-- | Convert a single field name to an item.
convertFieldNameM ::
  Maybe ItemKey.ItemKey ->
  Doc.Doc ->
  Maybe Text.Text ->
  Syntax.LFieldOcc Ghc.GhcPs ->
  Internal.ConvertM (Maybe (Located.Located Item.Item))
convertFieldNameM parentKey doc sig lFieldOcc =
  Internal.mkItemM
    (Annotation.getLocA lFieldOcc)
    parentKey
    (Just $ Internal.extractFieldOccName lFieldOcc)
    doc
    sig
    ItemKind.RecordField
