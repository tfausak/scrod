-- | Convert constructor declarations and record fields.
--
-- Handles both H98-style and GADT constructor declarations, including
-- signature extraction, documentation stripping, and record field
-- conversion.
module Scrod.Convert.FromGhc.Constructors where

import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified GHC.Hs.Extension as Ghc
import qualified GHC.Parser.Annotation as Annotation
import qualified GHC.Types.SrcLoc as SrcLoc
import qualified GHC.Utils.Outputable as Outputable
import qualified Language.Haskell.Syntax as Syntax
import qualified Language.Haskell.Syntax.Basic as SyntaxBasic
import qualified Scrod.Convert.FromGhc.Doc as GhcDoc
import qualified Scrod.Convert.FromGhc.Internal as Internal
import qualified Scrod.Convert.FromGhc.Names as Names
import qualified Scrod.Core.Doc as Doc
import qualified Scrod.Core.Item as Item
import qualified Scrod.Core.ItemKey as ItemKey
import qualified Scrod.Core.ItemKind as ItemKind
import qualified Scrod.Core.ItemName as ItemName
import qualified Scrod.Core.Located as Located
import qualified Scrod.Core.Since as Since

-- | Convert a constructor declaration.
-- GADT constructors can declare multiple names (e.g. @A, B :: Int -> T@),
-- so this emits one item per constructor name.
convertConDeclM ::
  Maybe ItemKey.ItemKey ->
  Maybe Text.Text ->
  Syntax.LConDecl Ghc.GhcPs ->
  Internal.ConvertM [Located.Located Item.Item]
convertConDeclM parentKey parentType lConDecl = do
  let conDecl = SrcLoc.unLoc lConDecl
      (conDoc, conSince) = extractConDeclDocAndSince conDecl
      conSig = extractConDeclSignature parentType conDecl
      conKind = constructorKind conDecl
      conNames = Names.extractConDeclNames conDecl
  fmap concat
    . traverse (convertOneConNameM (Annotation.getLocA lConDecl) parentKey conDoc conSince conSig conKind conDecl)
    $ NonEmpty.toList conNames

-- | Create items for a single constructor name, plus any record fields.
convertOneConNameM ::
  SrcLoc.SrcSpan ->
  Maybe ItemKey.ItemKey ->
  Doc.Doc ->
  Maybe Since.Since ->
  Maybe Text.Text ->
  ItemKind.ItemKind ->
  Syntax.ConDecl Ghc.GhcPs ->
  ItemName.ItemName ->
  Internal.ConvertM [Located.Located Item.Item]
convertOneConNameM srcSpan parentKey conDoc conSince conSig conKind conDecl conName = do
  result <-
    Internal.mkItemWithKeyM
      srcSpan
      parentKey
      (Just conName)
      conDoc
      conSince
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

-- | Extract documentation and @since from a constructor declaration.
extractConDeclDocAndSince :: Syntax.ConDecl Ghc.GhcPs -> (Doc.Doc, Maybe Since.Since)
extractConDeclDocAndSince conDecl = case conDecl of
  Syntax.ConDeclH98 {Syntax.con_doc = mDoc} ->
    maybe (Doc.Empty, Nothing) GhcDoc.convertLHsDoc mDoc
  Syntax.ConDeclGADT {Syntax.con_doc = mDoc} ->
    maybe (Doc.Empty, Nothing) GhcDoc.convertLHsDoc mDoc

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
          Just . Text.pack . Internal.showSDocShort . Outputable.ppr $
            conDecl
              { Syntax.con_doc = Nothing,
                Syntax.con_args = stripH98DetailsDocs args
              }
        Just parentType ->
          let forallDoc =
                if hasForall && not (null exTvs)
                  then
                    Outputable.hcat
                      [ Outputable.hsep (Outputable.text "forall" : fmap Outputable.ppr exTvs),
                        Outputable.text "."
                      ]
                  else Outputable.empty
              cxtDoc = case mbCxt of
                Nothing -> Outputable.empty
                Just ctx -> case SrcLoc.unLoc ctx of
                  [] -> Outputable.empty
                  [c] -> Outputable.hsep [Outputable.ppr c, Outputable.text "=>"]
                  cs ->
                    Outputable.hsep
                      [ Outputable.parens
                          (Outputable.hsep (Outputable.punctuate (Outputable.text ",") (fmap Outputable.ppr cs))),
                        Outputable.text "=>"
                      ]
              argsDoc = h98ArgsToDoc (stripH98DetailsDocs args)
              bodyDoc = case argsDoc of
                Nothing -> Outputable.text (Text.unpack parentType)
                Just ad -> Outputable.hsep [ad, Outputable.text "->", Outputable.text (Text.unpack parentType)]
           in Just . Text.pack . Internal.showSDocShort $
                Outputable.hsep [forallDoc, cxtDoc, bodyDoc]
  c@Syntax.ConDeclGADT {} ->
    let full =
          Text.pack . Internal.showSDocShort . Outputable.ppr $
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
      Outputable.hsep
        [ Outputable.ppr (Syntax.cdf_type l),
          Outputable.text "->",
          Outputable.ppr (Syntax.cdf_type r)
        ]
  Syntax.RecCon lFields ->
    let fields = SrcLoc.unLoc lFields
     in Just $ case fields of
          [f] ->
            Outputable.hsep [Outputable.text "{", Outputable.ppr f, Outputable.text "}"]
          (f : fs) ->
            Outputable.vcat $
              [Outputable.hsep [Outputable.text "{", Outputable.ppr f]]
                <> fmap (\fld -> Outputable.hsep [Outputable.text ",", Outputable.ppr fld]) fs
                <> [Outputable.text "}"]
          [] ->
            Outputable.text "{}"

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
  Syntax.PrefixCon fields -> convertPrefixArgsM parentKey fields
  Syntax.InfixCon l r -> convertPrefixArgsM parentKey [l, r]
  Syntax.RecCon lFields -> convertConDeclFieldsM parentKey (SrcLoc.unLoc lFields)

-- | Extract fields from GADT-style constructor details.
extractFieldsFromGADTDetailsM ::
  Maybe ItemKey.ItemKey ->
  Syntax.HsConDeclGADTDetails Ghc.GhcPs ->
  Internal.ConvertM [Located.Located Item.Item]
extractFieldsFromGADTDetailsM parentKey details = case details of
  Syntax.PrefixConGADT _ fields -> convertPrefixArgsM parentKey fields
  Syntax.RecConGADT _ lFields -> convertConDeclFieldsM parentKey (SrcLoc.unLoc lFields)

-- | Convert prefix constructor arguments to Argument items.
convertPrefixArgsM ::
  Maybe ItemKey.ItemKey ->
  [Syntax.HsConDeclField Ghc.GhcPs] ->
  Internal.ConvertM [Located.Located Item.Item]
convertPrefixArgsM parentKey = fmap Maybe.catMaybes . traverse (convertPrefixArgM parentKey)

-- | Convert a single prefix constructor argument to an Argument item.
convertPrefixArgM ::
  Maybe ItemKey.ItemKey ->
  Syntax.HsConDeclField Ghc.GhcPs ->
  Internal.ConvertM (Maybe (Located.Located Item.Item))
convertPrefixArgM parentKey field =
  let (argDoc, argSince) = maybe (Doc.Empty, Nothing) GhcDoc.convertLHsDoc $ Syntax.cdf_doc field
      sig =
        Just . Text.pack . Internal.showSDocShort $
          Outputable.hcat
            [ unpackednessDoc (Syntax.cdf_unpack field),
              strictnessDoc (Syntax.cdf_bang field),
              Outputable.ppr (Syntax.cdf_type field)
            ]
   in Internal.mkItemM
        (Annotation.getLocA (Syntax.cdf_type field))
        parentKey
        Nothing
        argDoc
        argSince
        sig
        ItemKind.Argument

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
      (doc, docSince) = maybe (Doc.Empty, Nothing) GhcDoc.convertLHsDoc $ Syntax.cdf_doc fieldSpec
      sig =
        Just . Text.pack . Internal.showSDocShort $
          Outputable.hcat
            [ unpackednessDoc (Syntax.cdf_unpack fieldSpec),
              strictnessDoc (Syntax.cdf_bang fieldSpec),
              Outputable.ppr (Syntax.cdf_type fieldSpec)
            ]
   in Maybe.catMaybes <$> traverse (convertFieldNameM parentKey doc docSince sig) (Syntax.cdrf_names recField)

-- | Convert a single field name to an item.
convertFieldNameM ::
  Maybe ItemKey.ItemKey ->
  Doc.Doc ->
  Maybe Since.Since ->
  Maybe Text.Text ->
  Syntax.LFieldOcc Ghc.GhcPs ->
  Internal.ConvertM (Maybe (Located.Located Item.Item))
convertFieldNameM parentKey doc docSince sig lFieldOcc =
  Internal.mkItemM
    (Annotation.getLocA lFieldOcc)
    parentKey
    (Just $ Internal.extractFieldOccName lFieldOcc)
    doc
    docSince
    sig
    ItemKind.RecordField

-- | Convert source unpackedness to its textual representation.
unpackednessDoc :: SyntaxBasic.SrcUnpackedness -> Outputable.SDoc
unpackednessDoc u = case u of
  SyntaxBasic.SrcUnpack -> Outputable.text "{-# UNPACK #-} "
  SyntaxBasic.SrcNoUnpack -> Outputable.text "{-# NOUNPACK #-} "
  SyntaxBasic.NoSrcUnpack -> Outputable.empty

-- | Convert source strictness to its textual representation.
strictnessDoc :: SyntaxBasic.SrcStrictness -> Outputable.SDoc
strictnessDoc s = case s of
  SyntaxBasic.SrcLazy -> Outputable.text "~"
  SyntaxBasic.SrcStrict -> Outputable.text "!"
  SyntaxBasic.NoSrcStrict -> Outputable.empty
