module Scrod.Unstable.Type.Export where

import qualified Data.Text as Text
import qualified Scrod.Unstable.Type.Doc as Doc
import qualified Scrod.Unstable.Type.ExportIdentifier as ExportIdentifier
import qualified Scrod.Unstable.Type.Json as Json
import qualified Scrod.Unstable.Type.Section as Section

-- | A single entry in a module's export list.
-- Mirrors GHC's IE type but simplified.
data Export
  = -- | A named export: variable, type/class, or module re-export.
    Identifier ExportIdentifier.ExportIdentifier
  | -- | Section heading: @-- * Section@
    Group Section.Section
  | -- | Inline documentation: @-- | Some doc@
    Doc Doc.Doc
  | -- | Named doc reference: @-- $chunkName@
    DocNamed Text.Text
  deriving (Eq, Ord, Show)

toJson :: Export -> Json.Json
toJson export = case export of
  Identifier i -> Json.tagged (Text.pack "Identifier") $ ExportIdentifier.toJson i
  Group s -> Json.tagged (Text.pack "Group") $ Section.toJson s
  Doc d -> Json.tagged (Text.pack "Doc") $ Doc.toJson d
  DocNamed t -> Json.tagged (Text.pack "DocNamed") $ Json.fromText t
