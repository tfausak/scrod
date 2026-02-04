module LegendaryChainsaw.Core.Doc where

import qualified Data.Text as Text
import qualified LegendaryChainsaw.Core.Example as Example
import qualified LegendaryChainsaw.Core.Header as Header
import qualified LegendaryChainsaw.Core.Hyperlink as Hyperlink
import qualified LegendaryChainsaw.Core.Identifier as Identifier
import qualified LegendaryChainsaw.Core.ModLink as ModLink
import qualified LegendaryChainsaw.Core.Picture as Picture
import qualified LegendaryChainsaw.Core.Table as Table

-- | Documentation AST.
data Doc
  = Empty
  | Append Doc Doc
  | String Text.Text
  | Paragraph Doc
  | Identifier Identifier.Identifier
  | Module (ModLink.ModLink Doc)
  | Emphasis Doc
  | Monospaced Doc
  | Bold Doc
  | UnorderedList [Doc]
  | OrderedList [(Int, Doc)]
  | DefList [(Doc, Doc)]
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
  deriving (Eq, Ord, Show)
