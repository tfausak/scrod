module Scrod.Type.Doc where

import qualified Data.Text as Text
import qualified Scrod.Type.Example as Example
import qualified Scrod.Type.Header as Header
import qualified Scrod.Type.Hyperlink as Hyperlink
import qualified Scrod.Type.Identifier as Identifier
import qualified Scrod.Type.ModLink as ModLink
import qualified Scrod.Type.Picture as Picture
import qualified Scrod.Type.Table as Table

-- | Documentation AST, simplified from Haddock's 'DocH'.
--
-- Differences from 'Documentation.Haddock.Types.DocH':
--   - No 'mod' type parameter (was always 'Void' in Scrod)
--   - No 'id' type parameter (always 'Identifier')
--   - No 'DocIdentifierUnchecked' constructor (never used)
--   - Uses 'Text' instead of 'String'
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

append :: Doc -> Doc -> Doc
append x y = case (x, y) of
  (Empty, _) -> y
  (_, Empty) -> x
  _ -> Append x y
