{-# LANGUAGE OverloadedStrings #-}

module Scrod.Html.Item where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Lucid
import qualified Numeric.Natural as Natural
import qualified Scrod.Html.Doc as HtmlDoc
import qualified Scrod.Type.Column as Column
import qualified Scrod.Type.Doc as Doc
import qualified Scrod.Type.Item as Item
import qualified Scrod.Type.ItemKey as ItemKey
import qualified Scrod.Type.ItemName as ItemName
import qualified Scrod.Type.Line as Line
import qualified Scrod.Type.Located as Located
import qualified Scrod.Type.Location as Location

-- | Convert a single Item to HTML.
toHtml :: Located.Located Item.Item -> Lucid.Html ()
toHtml (Located.MkLocated loc (Item.MkItem key _parentKey maybeName doc)) =
  Lucid.div_ attrs $
    nameHtml
      <> keyHtml
      <> locationHtml loc
      <> docHtml
  where
    attrs =
      [ Lucid.class_ "item",
        Lucid.id_ ("item-" <> Text.pack (show (ItemKey.value key)))
      ]

    nameHtml :: Lucid.Html ()
    nameHtml = case maybeName of
      Nothing -> mempty
      Just (ItemName.MkItemName name) ->
        Lucid.span_ [Lucid.class_ "item-name"] (Lucid.toHtml name)

    keyHtml :: Lucid.Html ()
    keyHtml =
      Lucid.span_ [Lucid.class_ "item-key"] $
        Lucid.toHtml ("#" <> Text.pack (show (ItemKey.value key)))

    docHtml :: Lucid.Html ()
    docHtml = case doc of
      Doc.Empty -> mempty
      _ -> Lucid.div_ [Lucid.class_ "item-doc"] (HtmlDoc.toHtml doc)

locationHtml :: Location.Location -> Lucid.Html ()
locationHtml (Location.MkLocation (Line.MkLine l) (Column.MkColumn c)) =
  Lucid.span_ [Lucid.class_ "item-location"] $
    Lucid.toHtml (" (line " <> Text.pack (show l) <> ", col " <> Text.pack (show c) <> ")")

-- | Render items with hierarchical structure based on parentKey.
-- Groups children under their parent items.
toHtmlHierarchical :: [Located.Located Item.Item] -> Lucid.Html ()
toHtmlHierarchical items =
  foldMap renderItem topLevelItems
  where
    -- Build a map from parent key to children
    childrenMap :: Map.Map Natural.Natural [Located.Located Item.Item]
    childrenMap = foldr addChild Map.empty items
      where
        addChild li acc = case Item.parentKey (Located.value li) of
          Nothing -> acc
          Just (ItemKey.MkItemKey pk) -> Map.insertWith (++) pk [li] acc

    -- Top-level items (no parent)
    topLevelItems :: [Located.Located Item.Item]
    topLevelItems = filter (isNothing . Item.parentKey . Located.value) items

    isNothing :: Maybe a -> Bool
    isNothing Nothing = True
    isNothing _ = False

    -- Render an item and its children
    renderItem :: Located.Located Item.Item -> Lucid.Html ()
    renderItem li =
      let key = ItemKey.value (Item.key (Located.value li))
          children = Map.findWithDefault [] key childrenMap
       in toHtml li
            <> if null children
              then mempty
              else Lucid.div_ [Lucid.class_ "item-children"] (foldMap renderItem children)
