{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Scrod.Core.Doc where

import qualified Data.Text as Text
import qualified GHC.Generics as Generics
import qualified Scrod.Core.Definition as Definition
import qualified Scrod.Core.Example as Example
import qualified Scrod.Core.Header as Header
import qualified Scrod.Core.Hyperlink as Hyperlink
import qualified Scrod.Core.Identifier as Identifier
import qualified Scrod.Core.ModLink as ModLink
import qualified Scrod.Core.NumberedItem as NumberedItem
import qualified Scrod.Core.Picture as Picture
import qualified Scrod.Core.Table as Table
import qualified Scrod.Json.ToJson as ToJson
import qualified Scrod.Schema as Schema

-- | Documentation AST.
data Doc
  = Empty
  | Append [Doc]
  | String Text.Text
  | Paragraph Doc
  | Identifier Identifier.Identifier
  | Module (ModLink.ModLink Doc)
  | Emphasis Doc
  | Monospaced Doc
  | Bold Doc
  | UnorderedList [Doc]
  | OrderedList [NumberedItem.NumberedItem Doc]
  | DefList [Definition.Definition Doc]
  | CodeBlock Doc
  | Hyperlink (Hyperlink.Hyperlink Doc)
  | Pic Picture.Picture
  | MathInline Text.Text
  | MathDisplay Text.Text
  | AName Text.Text
  | Property Text.Text
  | Examples [Example.Example]
  | Header (Header.Header Doc)
  | Table (Table.Table Doc)
  deriving (Eq, Generics.Generic, Ord, Show)
  deriving (ToJson.ToJson, Schema.ToSchema) via Generics.Generically Doc
