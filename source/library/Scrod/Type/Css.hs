{-# LANGUAGE OverloadedStrings #-}

module Scrod.Type.Css where

import qualified Clay
import qualified Clay.Media as Media
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText

-- | Default CSS stylesheet for HTML documentation output.
stylesheet :: Text.Text
stylesheet = LazyText.toStrict (Clay.render css)

css :: Clay.Css
css = do
  -- Reset
  Clay.star <> Clay.star Clay.# Clay.before <> Clay.star Clay.# Clay.after Clay.? do
    Clay.boxSizing Clay.borderBox

  -- Body
  Clay.body Clay.? do
    Clay.fontFamily ["system-ui", "-apple-system"] [Clay.sansSerif]
    Clay.lineHeight (Clay.unitless 1.6)
    Clay.maxWidth (Clay.px 900)
    Clay.margin Clay.nil Clay.auto Clay.nil Clay.auto
    Clay.padding (Clay.rem 2) (Clay.rem 2) (Clay.rem 2) (Clay.rem 2)
    Clay.color (Clay.parse "#333")

  -- Headings
  Clay.h1 Clay.? do
    Clay.borderBottom (Clay.px 2) Clay.solid (Clay.parse "#333")
    Clay.paddingBottom (Clay.rem 0.5)
    Clay.marginTop Clay.nil

  Clay.h2 Clay.? do
    Clay.borderBottom (Clay.px 1) Clay.solid (Clay.parse "#666")
    Clay.paddingBottom (Clay.rem 0.3)
    Clay.marginTop (Clay.rem 2)

  (Clay.h3 <> Clay.h4 <> Clay.h5 <> Clay.h6) Clay.? do
    Clay.marginTop (Clay.rem 1.5)

  -- Paragraphs
  Clay.p Clay.? do
    Clay.margin (Clay.rem 1) Clay.nil (Clay.rem 1) Clay.nil

  -- Code
  Clay.code Clay.? do
    Clay.background (Clay.parse "#f4f4f4")
    Clay.padding (Clay.em 0.2) (Clay.em 0.4) (Clay.em 0.2) (Clay.em 0.4)
    Clay.borderRadius (Clay.px 3) (Clay.px 3) (Clay.px 3) (Clay.px 3)
    Clay.fontFamily ["Consolas", "Monaco", "Menlo"] [Clay.monospace]
    Clay.fontSize (Clay.em 0.9)

  Clay.pre Clay.? do
    Clay.background (Clay.parse "#f4f4f4")
    Clay.padding (Clay.rem 1) (Clay.rem 1) (Clay.rem 1) (Clay.rem 1)
    Clay.borderRadius (Clay.px 5) (Clay.px 5) (Clay.px 5) (Clay.px 5)
    Clay.overflowX Clay.auto
    Clay.margin (Clay.rem 1) Clay.nil (Clay.rem 1) Clay.nil

  Clay.pre Clay.|> Clay.code Clay.? do
    Clay.background Clay.transparent
    Clay.padding Clay.nil Clay.nil Clay.nil Clay.nil

  -- Lists
  (Clay.ul <> Clay.ol) Clay.? do
    Clay.margin (Clay.rem 1) Clay.nil (Clay.rem 1) Clay.nil
    Clay.paddingLeft (Clay.rem 2)

  Clay.li Clay.? do
    Clay.margin (Clay.rem 0.25) Clay.nil (Clay.rem 0.25) Clay.nil

  -- Definition lists
  Clay.dl Clay.? do
    Clay.margin (Clay.rem 1) Clay.nil (Clay.rem 1) Clay.nil

  Clay.dt Clay.? do
    Clay.fontWeight Clay.bold
    Clay.marginTop (Clay.rem 0.5)

  Clay.dd Clay.? do
    Clay.marginLeft (Clay.rem 2)
    Clay.marginBottom (Clay.rem 0.5)

  -- Tables
  Clay.table Clay.? do
    Clay.borderCollapse Clay.collapse
    Clay.width (Clay.pct 100)
    Clay.margin (Clay.rem 1) Clay.nil (Clay.rem 1) Clay.nil

  (Clay.th <> Clay.td) Clay.? do
    Clay.border (Clay.px 1) Clay.solid (Clay.parse "#ddd")
    Clay.padding (Clay.rem 0.5) (Clay.rem 0.5) (Clay.rem 0.5) (Clay.rem 0.5)
    Clay.textAlign (Clay.other "left")

  Clay.th Clay.? do
    Clay.background (Clay.parse "#f4f4f4")
    Clay.fontWeight Clay.bold

  -- Links
  Clay.a Clay.? do
    Clay.color (Clay.parse "#0066cc")
    Clay.textDecoration Clay.none

  Clay.a Clay.# Clay.hover Clay.? do
    Clay.textDecoration Clay.underline

  -- Images
  Clay.img Clay.? do
    Clay.maxWidth (Clay.pct 100)
    Clay.height Clay.auto

  -- Module header
  ".module-header" Clay.? do
    Clay.marginBottom (Clay.rem 2)

  ".module-doc" Clay.? do
    Clay.margin (Clay.rem 1) Clay.nil (Clay.rem 1) Clay.nil

  -- Metadata section
  ".metadata" Clay.? do
    Clay.background (Clay.parse "#f9f9f9")
    Clay.borderLeft (Clay.px 4) Clay.solid (Clay.parse "#0066cc")
    Clay.padding (Clay.rem 1) (Clay.rem 1) (Clay.rem 1) (Clay.rem 1)
    Clay.margin (Clay.rem 1) Clay.nil (Clay.rem 1) Clay.nil

  ".metadata" Clay.|> Clay.dt Clay.? do
    Clay.display Clay.inline

  ".metadata" Clay.|> Clay.dd Clay.? do
    Clay.display Clay.inline
    Clay.marginLeft (Clay.rem 0.5)

  ".metadata" Clay.|> Clay.dd Clay.# Clay.after Clay.? do
    Clay.content (Clay.stringContent "")
    Clay.display Clay.block

  -- Exports section
  ".exports" Clay.? do
    Clay.margin (Clay.rem 2) Clay.nil (Clay.rem 2) Clay.nil

  ".export-group" Clay.? do
    Clay.margin (Clay.rem 1.5) Clay.nil (Clay.rem 1.5) Clay.nil

  ".export-group-title" Clay.? do
    Clay.fontWeight Clay.bold
    Clay.color (Clay.parse "#333")
    Clay.marginBottom (Clay.rem 0.5)

  ".export-list" Clay.? do
    Clay.listStyleType Clay.none
    Clay.paddingLeft Clay.nil

  ".export-list" Clay.|> Clay.li Clay.? do
    Clay.padding (Clay.rem 0.25) Clay.nil (Clay.rem 0.25) Clay.nil

  -- Items section
  ".items" Clay.? do
    Clay.margin (Clay.rem 2) Clay.nil (Clay.rem 2) Clay.nil

  ".item" Clay.? do
    Clay.margin (Clay.rem 1.5) Clay.nil (Clay.rem 1.5) Clay.nil
    Clay.padding (Clay.rem 1) (Clay.rem 1) (Clay.rem 1) (Clay.rem 1)
    Clay.background (Clay.parse "#fafafa")
    Clay.borderRadius (Clay.px 5) (Clay.px 5) (Clay.px 5) (Clay.px 5)
    Clay.borderLeft (Clay.px 4) Clay.solid (Clay.parse "#ddd")

  ".item-name" Clay.? do
    Clay.fontFamily ["Consolas", "Monaco", "Menlo"] [Clay.monospace]
    Clay.fontWeight Clay.bold
    Clay.fontSize (Clay.em 1.1)
    Clay.color (Clay.parse "#006600")

  ".item-key" Clay.? do
    Clay.color (Clay.parse "#999")
    Clay.fontSize (Clay.em 0.8)
    Clay.marginLeft (Clay.rem 0.5)

  ".item-doc" Clay.? do
    Clay.marginTop (Clay.rem 0.5)

  ".item-children" Clay.? do
    Clay.marginLeft (Clay.rem 1.5)
    Clay.marginTop (Clay.rem 0.5)
    Clay.borderLeft (Clay.px 2) Clay.solid (Clay.parse "#ddd")
    Clay.paddingLeft (Clay.rem 1)

  -- Identifiers and module links
  ".identifier" Clay.? do
    Clay.color (Clay.parse "#006600")
    Clay.fontFamily ["Consolas", "Monaco", "Menlo"] [Clay.monospace]

  ".module-link" Clay.? do
    Clay.color (Clay.parse "#660066")
    Clay.fontFamily ["Consolas", "Monaco", "Menlo"] [Clay.monospace]

  -- Warning
  ".warning" Clay.? do
    Clay.background (Clay.parse "#fff3cd")
    Clay.borderLeft (Clay.px 4) Clay.solid (Clay.parse "#ffc107")
    Clay.padding (Clay.rem 1) (Clay.rem 1) (Clay.rem 1) (Clay.rem 1)
    Clay.margin (Clay.rem 1) Clay.nil (Clay.rem 1) Clay.nil

  ".warning-category" Clay.? do
    Clay.fontWeight Clay.bold
    Clay.color (Clay.parse "#856404")

  -- Since
  ".since" Clay.? do
    Clay.color (Clay.parse "#666")
    Clay.fontSize (Clay.em 0.9)

  -- Examples
  ".examples" Clay.? do
    Clay.background (Clay.parse "#fffef0")
    Clay.borderLeft (Clay.px 4) Clay.solid (Clay.parse "#e6db74")
    Clay.padding (Clay.rem 1) (Clay.rem 1) (Clay.rem 1) (Clay.rem 1)
    Clay.margin (Clay.rem 1) Clay.nil (Clay.rem 1) Clay.nil

  ".example" Clay.? do
    Clay.margin (Clay.rem 0.5) Clay.nil (Clay.rem 0.5) Clay.nil

  ".example-expression" Clay.? do
    Clay.fontFamily ["Consolas", "Monaco", "Menlo"] [Clay.monospace]

  ".example-expression" Clay.# Clay.before Clay.? do
    Clay.content (Clay.stringContent ">>> ")
    Clay.color (Clay.parse "#999")

  ".example-result" Clay.? do
    Clay.fontFamily ["Consolas", "Monaco", "Menlo"] [Clay.monospace]
    Clay.color (Clay.parse "#666")
    Clay.paddingLeft (Clay.rem 1)

  -- Property
  ".property" Clay.? do
    Clay.background (Clay.parse "#f0f8ff")
    Clay.borderLeft (Clay.px 4) Clay.solid (Clay.parse "#4169e1")
    Clay.padding (Clay.rem 1) (Clay.rem 1) (Clay.rem 1) (Clay.rem 1)
    Clay.margin (Clay.rem 1) Clay.nil (Clay.rem 1) Clay.nil

  -- Math
  ".math-inline" Clay.? do
    Clay.fontStyle Clay.italic

  ".math-display" Clay.? do
    Clay.display Clay.block
    Clay.textAlign Clay.center
    Clay.margin (Clay.rem 1) Clay.nil (Clay.rem 1) Clay.nil
    Clay.fontStyle Clay.italic

  -- Extensions
  ".extensions" Clay.? do
    Clay.margin (Clay.rem 1) Clay.nil (Clay.rem 1) Clay.nil

  ".extension" Clay.? do
    Clay.display Clay.inlineBlock
    Clay.margin (Clay.rem 0.25) (Clay.rem 0.25) (Clay.rem 0.25) (Clay.rem 0.25)
    Clay.padding (Clay.rem 0.25) (Clay.rem 0.5) (Clay.rem 0.25) (Clay.rem 0.5)
    Clay.background (Clay.parse "#e8e8e8")
    Clay.borderRadius (Clay.px 3) (Clay.px 3) (Clay.px 3) (Clay.px 3)
    Clay.fontFamily ["Consolas", "Monaco", "Menlo"] [Clay.monospace]
    Clay.fontSize (Clay.em 0.85)

  ".extension-disabled" Clay.? do
    Clay.background (Clay.parse "#ffebeb")
    Clay.textDecoration Clay.lineThrough

  -- Print media query
  Clay.query Media.print [] $ do
    Clay.body Clay.? do
      Clay.maxWidth Clay.none
      Clay.padding Clay.nil Clay.nil Clay.nil Clay.nil

    ".metadata" Clay.? do
      Clay.background Clay.transparent
      Clay.border (Clay.px 1) Clay.solid (Clay.parse "#ccc")

  -- Mobile media query
  Clay.query Media.screen [Media.maxWidth (Clay.px 600)] $ do
    Clay.body Clay.? do
      Clay.padding (Clay.rem 1) (Clay.rem 1) (Clay.rem 1) (Clay.rem 1)
