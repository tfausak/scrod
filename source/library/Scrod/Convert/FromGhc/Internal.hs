-- | Shared types and utilities for the GHC-to-Scrod conversion.
--
-- Provides the 'ConvertM' state monad, item creation primitives,
-- location conversion, name extraction, warning conversion, and
-- document append. Every other @Scrod.Convert.FromGhc.*@ submodule
-- imports this module.
module Scrod.Convert.FromGhc.Internal where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Documentation.Haddock.Types as Haddock
import qualified GHC.Data.FastString as FastString
import qualified GHC.Hs.Doc as HsDoc
import qualified GHC.Hs.Extension as Ghc
import GHC.Hs.Type ()
import qualified GHC.Types.Name.Reader as Reader
import qualified GHC.Types.SourceText as SourceText
import qualified GHC.Types.SrcLoc as SrcLoc
import qualified GHC.Unit.Module.Warnings as Warnings
import qualified GHC.Utils.Outputable as Outputable
import qualified Language.Haskell.Syntax as Syntax
import qualified Numeric.Natural as Natural
import qualified Scrod.Core.Category as Category
import qualified Scrod.Core.Column as Column
import qualified Scrod.Core.Doc as Doc
import qualified Scrod.Core.Item as Item
import qualified Scrod.Core.ItemKey as ItemKey
import qualified Scrod.Core.ItemKind as ItemKind
import qualified Scrod.Core.ItemName as ItemName
import qualified Scrod.Core.Line as Line
import qualified Scrod.Core.Located as Located
import qualified Scrod.Core.Location as Location
import qualified Scrod.Core.ModuleName as ModuleName
import qualified Scrod.Core.PackageName as PackageName
import qualified Scrod.Core.Since as Since
import qualified Scrod.Core.Version as Version
import qualified Scrod.Core.Visibility as Visibility
import qualified Scrod.Core.Warning as Warning

-- | State for tracking item keys during conversion.
newtype ConversionState = MkConversionState
  { nextKey :: Natural.Natural
  }

-- | Initial conversion state.
initialState :: ConversionState
initialState = MkConversionState {nextKey = 0}

-- | Allocate a new key from the state.
allocateKey :: ConversionState -> (ItemKey.ItemKey, ConversionState)
allocateKey s =
  let k = nextKey s
   in (ItemKey.MkItemKey k, s {nextKey = k + 1})

-- | Monad for item conversion with auto-incrementing keys.
type ConvertM a = State.State ConversionState a

-- | Allocate a new unique key.
allocateKeyM :: ConvertM ItemKey.ItemKey
allocateKeyM = State.state allocateKey

-- | Run the conversion monad and extract the result.
runConvert :: ConvertM a -> a
runConvert = flip State.evalState initialState

-- | Convert GHC module name to our 'ModuleName' type.
moduleNameFromGhc :: Syntax.ModuleName -> ModuleName.ModuleName
moduleNameFromGhc =
  ModuleName.MkModuleName
    . Text.pack
    . Syntax.moduleNameString

-- | Convert GHC Located to our 'Located' type.
locatedFromGhc :: SrcLoc.Located a -> Maybe (Located.Located a)
locatedFromGhc (SrcLoc.L srcSpan a) = do
  location <- locationFromSrcSpan srcSpan
  Just
    Located.MkLocated
      { Located.location = location,
        Located.value = a
      }

-- | Convert SrcSpan to our 'Location' type.
locationFromSrcSpan :: SrcLoc.SrcSpan -> Maybe Location.Location
locationFromSrcSpan srcSpan = case srcSpan of
  SrcLoc.RealSrcSpan realSrcSpan _ ->
    Just
      Location.MkLocation
        { Location.line = Line.MkLine . fromIntegral $ SrcLoc.srcSpanStartLine realSrcSpan,
          Location.column = Column.MkColumn . fromIntegral $ SrcLoc.srcSpanStartCol realSrcSpan
        }
  SrcLoc.UnhelpfulSpan _ -> Nothing

-- | Convert GHC WarningTxt to our 'Warning' type.
warningTxtToWarning :: Warnings.WarningTxt Ghc.GhcPs -> Warning.Warning
warningTxtToWarning warningTxt =
  Warning.MkWarning
    { Warning.category = categoryFromGhc $ Warnings.warningTxtCategory warningTxt,
      Warning.value =
        Text.intercalate (Text.singleton '\n')
          . fmap extractMessage
          $ Warnings.warningTxtMessage warningTxt
    }

-- | Render an SDoc with a short line length to encourage line breaks.
showSDocShort :: Outputable.SDoc -> String
showSDocShort =
  Outputable.renderWithContext
    Outputable.defaultSDocContext {Outputable.sdocLineLength = 40}

-- | Convert GHC WarningCategory to our 'Category' type.
categoryFromGhc :: Warnings.WarningCategory -> Category.Category
categoryFromGhc =
  Category.MkCategory
    . Text.pack
    . Outputable.showSDocUnsafe
    . Outputable.ppr

-- | Extract message text from a located doc string.
extractMessage ::
  SrcLoc.GenLocated l (HsDoc.WithHsDocIdentifiers SourceText.StringLiteral Ghc.GhcPs) ->
  Text.Text
extractMessage =
  Text.pack
    . FastString.unpackFS
    . SourceText.sl_fs
    . HsDoc.hsDocString
    . SrcLoc.unLoc

-- | Extract name from RdrName.
extractRdrName :: SrcLoc.GenLocated l Reader.RdrName -> Text.Text
extractRdrName =
  Text.pack
    . Outputable.showSDocUnsafe
    . Outputable.ppr
    . SrcLoc.unLoc

-- | Extract name from an identifier.
extractIdPName :: Syntax.LIdP Ghc.GhcPs -> ItemName.ItemName
extractIdPName = ItemName.MkItemName . extractRdrName

-- | Extract name from a field occurrence.
extractFieldOccName :: Syntax.LFieldOcc Ghc.GhcPs -> ItemName.ItemName
extractFieldOccName lFieldOcc = case SrcLoc.unLoc lFieldOcc of
  Syntax.FieldOcc _ lbl -> ItemName.MkItemName $ extractRdrName lbl

-- | Append two 'Doc' values.
appendDoc :: Doc.Doc -> Doc.Doc -> Doc.Doc
appendDoc Doc.Empty d = d
appendDoc d Doc.Empty = d
appendDoc d1 d2 = Doc.Append [d1, d2]

-- | Combine two 'Maybe Since.Since' values, preferring the first.
appendSince :: Maybe Since.Since -> Maybe Since.Since -> Maybe Since.Since
appendSince a b = case a of
  Just _ -> a
  Nothing -> b

-- | Convert a Haddock MetaSince to our 'Since'.
metaSinceToSince :: Haddock.MetaSince -> Maybe Since.Since
metaSinceToSince metaSince = do
  versionNE <- NonEmpty.nonEmpty $ Haddock.sinceVersion metaSince
  Just
    Since.MkSince
      { Since.package =
          PackageName.MkPackageName . Text.pack
            <$> Haddock.sincePackage metaSince,
        Since.version =
          Version.MkVersion $ fmap (fromIntegral :: Int -> Natural.Natural) versionNE
      }

-- | Set the parent key on a located item.
setParentKey :: ItemKey.ItemKey -> Located.Located Item.Item -> Located.Located Item.Item
setParentKey pk li =
  li {Located.value = (Located.value li) {Item.parentKey = Just pk}}

-- | Create an Item from a source span with the given properties.
mkItemM ::
  SrcLoc.SrcSpan ->
  Maybe ItemKey.ItemKey ->
  Maybe ItemName.ItemName ->
  Doc.Doc ->
  Maybe Since.Since ->
  Maybe Text.Text ->
  ItemKind.ItemKind ->
  ConvertM (Maybe (Located.Located Item.Item))
mkItemM srcSpan parentKey itemName doc itemSince sig itemKind =
  fmap fst <$> mkItemWithKeyM srcSpan parentKey itemName doc itemSince sig itemKind

-- | Extract source locations from module declarations using the given
-- per-declaration extractor. Common skeleton for pragma location extraction.
extractDeclLocations ::
  (Syntax.LHsDecl Ghc.GhcPs -> [Location.Location]) ->
  SrcLoc.Located (Syntax.HsModule Ghc.GhcPs) ->
  Set.Set Location.Location
extractDeclLocations extractFromDecl lHsModule =
  let hsModule = SrcLoc.unLoc lHsModule
      decls = Syntax.hsmodDecls hsModule
   in Set.fromList $ concatMap extractFromDecl decls

-- | Create an Item and return both the item and its allocated key.
mkItemWithKeyM ::
  SrcLoc.SrcSpan ->
  Maybe ItemKey.ItemKey ->
  Maybe ItemName.ItemName ->
  Doc.Doc ->
  Maybe Since.Since ->
  Maybe Text.Text ->
  ItemKind.ItemKind ->
  ConvertM (Maybe (Located.Located Item.Item, ItemKey.ItemKey))
mkItemWithKeyM srcSpan parentKey itemName doc itemSince sig itemKind =
  case locationFromSrcSpan srcSpan of
    Nothing -> pure Nothing
    Just location -> do
      key <- allocateKeyM
      pure $
        Just
          ( Located.MkLocated
              { Located.location = location,
                Located.value =
                  Item.MkItem
                    { Item.key = key,
                      Item.kind = itemKind,
                      Item.parentKey = parentKey,
                      Item.name = itemName,
                      Item.documentation = doc,
                      Item.since = itemSince,
                      Item.signature = sig,
                      Item.visibility = Visibility.Exported
                    }
              },
            key
          )
