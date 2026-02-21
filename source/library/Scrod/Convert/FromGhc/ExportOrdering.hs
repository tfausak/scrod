-- | Reorder items according to the module's export list.
--
-- When an export list is present, items are reordered so that exported
-- items come first (in export-list order), followed by implicit items,
-- then unexported items. Export-list-only entries (section headings,
-- inline docs, re-exports with no matching declaration) become
-- synthetic items.
--
-- When no export list is present, items are returned unchanged.
module Scrod.Convert.FromGhc.ExportOrdering (reorderByExports) where

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Numeric.Natural as Natural
import qualified Scrod.Convert.FromGhc.Internal as Internal
import qualified Scrod.Core.Category as Category
import qualified Scrod.Core.Column as Column
import qualified Scrod.Core.Doc as Doc
import qualified Scrod.Core.Export as Export
import qualified Scrod.Core.ExportIdentifier as ExportIdentifier
import qualified Scrod.Core.ExportName as ExportName
import qualified Scrod.Core.ExportNameKind as ExportNameKind
import qualified Scrod.Core.Item as Item
import qualified Scrod.Core.ItemKey as ItemKey
import qualified Scrod.Core.ItemKind as ItemKind
import qualified Scrod.Core.ItemName as ItemName
import qualified Scrod.Core.Line as Line
import qualified Scrod.Core.Located as Located
import qualified Scrod.Core.Location as Location
import qualified Scrod.Core.Section as Section
import qualified Scrod.Core.Visibility as Visibility
import qualified Scrod.Core.Warning as Warning

-- | Reorder items according to the module's export list.
--
-- When @Nothing@, return items unchanged. When @Just exports@,
-- reorder items to match the export list and create synthetic items
-- for export-list-only entries (sections, docs, re-exports).
reorderByExports ::
  Maybe [Export.Export] ->
  [Located.Located Item.Item] ->
  [Located.Located Item.Item]
reorderByExports mExports items = case mExports of
  Nothing -> items
  Just [] -> items
  Just exports ->
    let nameMap = topLevelNameMap items
        nextKey = nextItemKey items
        (exportedItems, usedKeys, _) = walkExports exports nameMap Set.empty nextKey
        implicitItems = collectImplicit usedKeys items
        usedKeys2 = foldr (Set.insert . Item.key . Located.value) usedKeys implicitItems
        unexportedItems = collectUnexported usedKeys2 items
        usedKeys3 = foldr (Set.insert . Item.key . Located.value) usedKeys2 unexportedItems
        remainingItems = collectRemaining usedKeys3 items
        childItems = filter (Maybe.isJust . Item.parentKey . Located.value) items
     in exportedItems <> implicitItems <> unexportedItems <> remainingItems <> childItems

-- | Build a map from top-level item names to located items.
-- For items whose names contain type variables (indicated by a space),
-- both the full name and the base name (first word) are indexed.
topLevelNameMap :: [Located.Located Item.Item] -> Map.Map Text.Text (Located.Located Item.Item)
topLevelNameMap items =
  Map.fromList
    [ entry
    | li <- items,
      Maybe.isNothing (Item.parentKey (Located.value li)),
      Just n <- [Item.name (Located.value li)],
      let full = ItemName.unwrap n,
      entry <- (full, li) : [(base, li) | Just base <- [Internal.baseItemName full]]
    ]

-- | Compute the next available item key (one past the maximum).
nextItemKey :: [Located.Located Item.Item] -> Natural.Natural
nextItemKey items = case items of
  [] -> 0
  _ -> 1 + maximum (fmap (ItemKey.unwrap . Item.key . Located.value) items)

-- | Walk the export list, emitting items in export order. The @used@
-- set tracks item keys already emitted to avoid duplicates. Returns
-- the emitted items, the final used set, and the next available
-- synthetic key.
walkExports ::
  [Export.Export] ->
  Map.Map Text.Text (Located.Located Item.Item) ->
  Set.Set ItemKey.ItemKey ->
  Natural.Natural ->
  ([Located.Located Item.Item], Set.Set ItemKey.ItemKey, Natural.Natural)
walkExports exports nameMap used nextKey = case exports of
  [] -> ([], used, nextKey)
  e : es -> case e of
    Export.Identifier ident ->
      let name = ExportName.name (ExportIdentifier.name ident)
       in case Map.lookup name nameMap of
            Just li
              | not (Set.member (Item.key (Located.value li)) used) ->
                  let meta = exportMetadataItems ident nextKey
                      nextKey2 = nextKey + fromIntegral (length meta)
                      used2 = Set.insert (Item.key (Located.value li)) used
                      (rest, used3, nextKey3) = walkExports es nameMap used2 nextKey2
                   in (li : meta <> rest, used3, nextKey3)
            Just _ ->
              -- Duplicate export: emit only metadata, skip the item.
              let meta = exportMetadataItems ident nextKey
                  nextKey2 = nextKey + fromIntegral (length meta)
                  (rest, used2, nextKey3) = walkExports es nameMap used nextKey2
               in (meta <> rest, used2, nextKey3)
            Nothing ->
              let (unresolvedItem, nextKey2) = mkUnresolvedExport ident nextKey
                  meta = exportMetadataItems ident nextKey2
                  nextKey3 = nextKey2 + fromIntegral (length meta)
                  (rest, used2, nextKey4) = walkExports es nameMap used nextKey3
               in (unresolvedItem : meta <> rest, used2, nextKey4)
    Export.Group section ->
      let (sectionItem, nextKey2) = mkSectionItem section nextKey
          (rest, used2, nextKey3) = walkExports es nameMap used nextKey2
       in (sectionItem : rest, used2, nextKey3)
    Export.Doc doc ->
      let (docItem, nextKey2) = mkDocItem doc nextKey
          (rest, used2, nextKey3) = walkExports es nameMap used nextKey2
       in (docItem : rest, used2, nextKey3)
    Export.DocNamed name ->
      let (docItem, nextKey2) = mkDocNamedItem name nextKey
          (rest, used2, nextKey3) = walkExports es nameMap used nextKey2
       in (docItem : rest, used2, nextKey3)

-- | Collect implicit items that haven't been used yet.
collectImplicit ::
  Set.Set ItemKey.ItemKey ->
  [Located.Located Item.Item] ->
  [Located.Located Item.Item]
collectImplicit usedKeys =
  filter
    ( \li ->
        let item = Located.value li
         in Maybe.isNothing (Item.parentKey item)
              && Item.visibility item == Visibility.Implicit
              && not (Set.member (Item.key item) usedKeys)
    )

-- | Collect unexported top-level items that haven't been used.
collectUnexported ::
  Set.Set ItemKey.ItemKey ->
  [Located.Located Item.Item] ->
  [Located.Located Item.Item]
collectUnexported usedKeys =
  filter
    ( \li ->
        let item = Located.value li
         in Maybe.isNothing (Item.parentKey item)
              && Item.visibility item == Visibility.Unexported
              && not (Set.member (Item.key item) usedKeys)
    )

-- | Catch-all for any top-level items not captured by the above
-- collectors. This cannot happen in practice today, but guards against
-- future drift between Visibility and ExportOrdering.
collectRemaining ::
  Set.Set ItemKey.ItemKey ->
  [Located.Located Item.Item] ->
  [Located.Located Item.Item]
collectRemaining usedKeys =
  filter
    ( \li ->
        let item = Located.value li
         in Maybe.isNothing (Item.parentKey item)
              && not (Set.member (Item.key item) usedKeys)
    )

-- | Create synthetic items for export-level doc and/or warning metadata
-- on an identifier.
exportMetadataItems :: ExportIdentifier.ExportIdentifier -> Natural.Natural -> [Located.Located Item.Item]
exportMetadataItems ident startKey =
  let warningItems = case ExportIdentifier.warning ident of
        Nothing -> []
        Just w ->
          [ mkSyntheticItem
              startKey
              Nothing
              (warningToDoc w)
              Nothing
              ItemKind.DocumentationChunk
          ]
      warningCount :: Natural.Natural
      warningCount = fromIntegral (length warningItems)
      docItems = case ExportIdentifier.doc ident of
        Nothing -> []
        Just d ->
          [ mkSyntheticItem
              (startKey + warningCount)
              Nothing
              d
              Nothing
              ItemKind.DocumentationChunk
          ]
   in warningItems <> docItems

-- | Convert a warning to a doc paragraph for inline display.
warningToDoc :: Warning.Warning -> Doc.Doc
warningToDoc w =
  Doc.Paragraph
    . Doc.Bold
    . Doc.Append
    $ [ Doc.String (Text.pack "Warning"),
        Doc.String (Text.pack " ("),
        Doc.String (Category.unwrap (Warning.category w)),
        Doc.String (Text.pack "): "),
        Doc.String (Warning.value w)
      ]

-- | Create an item for an unresolved export (no matching declaration).
-- When the export has an explicit namespace keyword (e.g.
-- @module A (type B) where@), it informs the 'ItemKind' instead of
-- being stored in the signature field. Module re-exports keep
-- @"module"@ in the signature since there is no distinct item kind
-- for them.
mkUnresolvedExport ::
  ExportIdentifier.ExportIdentifier ->
  Natural.Natural ->
  (Located.Located Item.Item, Natural.Natural)
mkUnresolvedExport ident nextKey =
  let exportName = ExportIdentifier.name ident
      name = ExportName.name exportName
      (kind, sig) = case ExportName.kind exportName of
        Just ExportNameKind.Module -> (ItemKind.UnresolvedExport, Just (Text.pack "module"))
        Just ExportNameKind.Pattern -> (ItemKind.PatternSynonym, Nothing)
        Just ExportNameKind.Type -> (ItemKind.TypeSynonym, Nothing)
        Nothing -> (ItemKind.UnresolvedExport, Nothing)
   in ( mkSyntheticItem
          nextKey
          (Just (ItemName.MkItemName name))
          Doc.Empty
          sig
          kind,
        nextKey + 1
      )

-- | Create a section heading item from an export group.
mkSectionItem ::
  Section.Section ->
  Natural.Natural ->
  (Located.Located Item.Item, Natural.Natural)
mkSectionItem section nextKey =
  let hdr = Section.header section
      doc = Doc.Header hdr
   in ( mkSyntheticItem nextKey Nothing doc Nothing ItemKind.DocumentationChunk,
        nextKey + 1
      )

-- | Create a documentation item from an inline export doc.
mkDocItem ::
  Doc.Doc ->
  Natural.Natural ->
  (Located.Located Item.Item, Natural.Natural)
mkDocItem doc nextKey =
  ( mkSyntheticItem nextKey Nothing doc Nothing ItemKind.DocumentationChunk,
    nextKey + 1
  )

-- | Create a documentation item from an unresolved named doc reference.
mkDocNamedItem ::
  Text.Text ->
  Natural.Natural ->
  (Located.Located Item.Item, Natural.Natural)
mkDocNamedItem name nextKey =
  ( mkSyntheticItem nextKey chunkName Doc.Empty Nothing ItemKind.DocumentationChunk,
    nextKey + 1
  )
  where
    chunkName = Just . ItemName.MkItemName $ Text.pack "$" <> name

-- | Create a synthetic item with a given key, not tied to any source
-- location.
mkSyntheticItem ::
  Natural.Natural ->
  Maybe ItemName.ItemName ->
  Doc.Doc ->
  Maybe Text.Text ->
  ItemKind.ItemKind ->
  Located.Located Item.Item
mkSyntheticItem key itemName doc sig kind =
  Located.MkLocated
    { Located.location =
        Location.MkLocation
          { Location.line = Line.MkLine 0,
            Location.column = Column.MkColumn 0
          },
      Located.value =
        Item.MkItem
          { Item.key = ItemKey.MkItemKey key,
            Item.kind = kind,
            Item.parentKey = Nothing,
            Item.name = itemName,
            Item.documentation = doc,
            Item.since = Nothing,
            Item.signature = sig,
            Item.visibility = Visibility.Exported
          }
    }
