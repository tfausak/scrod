-- | Compute visibility for each item based on the module's export list.
module Scrod.Convert.FromGhc.Visibility (computeVisibility) where

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Scrod.Convert.FromGhc.Internal as Internal
import qualified Scrod.Core.Export as Export
import qualified Scrod.Core.ExportIdentifier as ExportIdentifier
import qualified Scrod.Core.ExportName as ExportName
import qualified Scrod.Core.Item as Item
import qualified Scrod.Core.ItemKey as ItemKey
import qualified Scrod.Core.ItemKind as ItemKind
import qualified Scrod.Core.ItemName as ItemName
import qualified Scrod.Core.Located as Located
import qualified Scrod.Core.Subordinates as Subordinates
import qualified Scrod.Core.Visibility as Visibility

-- | Whether an item kind is always visible regardless of the export list.
isAlwaysVisible :: ItemKind.ItemKind -> Bool
isAlwaysVisible k = case k of
  ItemKind.ClassInstance -> True
  ItemKind.CompletePragma -> True
  ItemKind.StandaloneDeriving -> True
  ItemKind.DerivedInstance -> True
  ItemKind.Rule -> True
  ItemKind.Default -> True
  ItemKind.Annotation -> True
  ItemKind.Splice -> True
  _ -> False

-- | Compute visibility for each item based on the module's export list.
computeVisibility ::
  Maybe [Export.Export] ->
  [Located.Located Item.Item] ->
  [Located.Located Item.Item]
computeVisibility Nothing items = fmap setImplicit items
computeVisibility (Just exports) items =
  let exportedNames = extractExportedNames exports
      wildcardParentKeys = extractWildcardParentKeys exports items
      exportedParentSubs = extractExportedParentSubs exports items
   in fmap (classifyItem exportedNames wildcardParentKeys exportedParentSubs) items
  where
    classifyItem ::
      Set.Set Text.Text ->
      Set.Set ItemKey.ItemKey ->
      Map.Map ItemKey.ItemKey (Maybe Subordinates.Subordinates) ->
      Located.Located Item.Item ->
      Located.Located Item.Item
    classifyItem exportedNames wildcardKeys parentSubs li =
      let item = Located.value li
       in if isAlwaysVisible (Item.kind item)
            then setVisibility Visibility.Implicit li
            else case Item.parentKey item of
              Just pk
                | Set.member pk wildcardKeys ->
                    setVisibility Visibility.Exported li
                | Just subs <- Map.lookup pk parentSubs ->
                    classifyChild subs item li
              _ ->
                case Item.name item of
                  Just n
                    | nameIsExported (ItemName.unwrap n) exportedNames ->
                        setVisibility Visibility.Exported li
                  _ -> setVisibility Visibility.Unexported li

    -- Check if a name (possibly containing type variables) matches
    -- an exported name. Tries the full name first, then the base
    -- name (first word) for names with type variables.
    nameIsExported :: Text.Text -> Set.Set Text.Text -> Bool
    nameIsExported full names =
      Set.member full names
        || case Internal.baseItemName full of
          Just w -> Set.member w names
          Nothing -> False

    -- Classify a child of an exported parent based on subordinate
    -- restrictions. Non-traditional subordinates (e.g. associated type
    -- families) are always exported. Traditional subordinates
    -- (constructors, record fields, class methods) follow the export
    -- list's subordinate restrictions.
    classifyChild ::
      Maybe Subordinates.Subordinates ->
      Item.Item ->
      Located.Located Item.Item ->
      Located.Located Item.Item
    classifyChild subs item li
      | not (ItemKind.isTraditionalSubordinate (Item.kind item)) =
          setVisibility Visibility.Exported li
      | otherwise = case subs of
          Nothing -> setVisibility Visibility.Unexported li
          Just (Subordinates.MkSubordinates True _) ->
            setVisibility Visibility.Exported li
          Just (Subordinates.MkSubordinates False explicit) ->
            case Item.name item of
              Just n
                | Set.member (ItemName.unwrap n) explicitNames ->
                    setVisibility Visibility.Exported li
                where
                  explicitNames = Set.fromList $ fmap ExportName.name explicit
              _ -> setVisibility Visibility.Unexported li

-- | When there is no export list, tag implicit items and leave everything
-- else as 'Exported' (the default set by 'mkItemM').
setImplicit :: Located.Located Item.Item -> Located.Located Item.Item
setImplicit li =
  let item = Located.value li
   in if isAlwaysVisible (Item.kind item)
        then setVisibility Visibility.Implicit li
        else li

-- | Set the visibility field on a located item.
setVisibility :: Visibility.Visibility -> Located.Located Item.Item -> Located.Located Item.Item
setVisibility v li =
  li {Located.value = (Located.value li) {Item.visibility = v}}

-- | Extract the set of names that are directly exported (including
-- explicitly named subordinates like @Foo(Bar, Baz)@).
extractExportedNames :: [Export.Export] -> Set.Set Text.Text
extractExportedNames = foldMap go
  where
    go :: Export.Export -> Set.Set Text.Text
    go (Export.Identifier ident) =
      let name = ExportName.name (ExportIdentifier.name ident)
          explicitSubs = case ExportIdentifier.subordinates ident of
            Nothing -> Set.empty
            Just (Subordinates.MkSubordinates _ explicit) ->
              Set.fromList $ fmap ExportName.name explicit
       in Set.insert name explicitSubs
    go _ = Set.empty

-- | Extract the set of parent item keys whose children are exported via
-- a @(..)@ wildcard.
extractWildcardParentKeys ::
  [Export.Export] ->
  [Located.Located Item.Item] ->
  Set.Set ItemKey.ItemKey
extractWildcardParentKeys exports items =
  let wildcardNames =
        Set.fromList
          [ ExportName.name (ExportIdentifier.name ident)
          | Export.Identifier ident <- exports,
            Just (Subordinates.MkSubordinates True _) <- [ExportIdentifier.subordinates ident]
          ]
   in Set.fromList
        . Maybe.mapMaybe (\n -> Map.lookup n (topLevelNameToKey items))
        $ Set.toList wildcardNames

-- | Extract a map from exported parent item keys to their subordinate
-- restrictions. The value is 'Nothing' when the parent is exported with
-- no subordinates at all (e.g. @Foo@), or 'Just' when subordinates are
-- present (e.g. @Foo(..)@ or @Foo(Bar)@).
extractExportedParentSubs ::
  [Export.Export] ->
  [Located.Located Item.Item] ->
  Map.Map ItemKey.ItemKey (Maybe Subordinates.Subordinates)
extractExportedParentSubs exports items =
  let exportSubs =
        [ (ExportName.name (ExportIdentifier.name ident), ExportIdentifier.subordinates ident)
        | Export.Identifier ident <- exports
        ]
      nk = topLevelNameToKey items
   in Map.fromList
        . Maybe.mapMaybe (\(n, subs) -> (\k -> (k, subs)) <$> Map.lookup n nk)
        $ exportSubs

-- | Build a map from top-level item names to their keys.
-- For items whose names contain type variables (indicated by a space),
-- both the full name and the base name (first word) are indexed.
topLevelNameToKey :: [Located.Located Item.Item] -> Map.Map Text.Text ItemKey.ItemKey
topLevelNameToKey items =
  Map.fromList
    [ entry
    | li <- items,
      Maybe.isNothing (Item.parentKey (Located.value li)),
      Just n <- [Item.name (Located.value li)],
      let full = ItemName.unwrap n,
      let k = Item.key (Located.value li),
      entry <- (full, k) : [(base, k) | Just base <- [Internal.baseItemName full]]
    ]
