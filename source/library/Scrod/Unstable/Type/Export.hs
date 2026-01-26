module Scrod.Unstable.Type.Export where

import qualified Data.Text as Text
import qualified Scrod.Unstable.Type.Doc as Doc
import qualified Scrod.Unstable.Type.ExportName as ExportName
import qualified Scrod.Unstable.Type.Section as Section
import qualified Scrod.Unstable.Type.Subordinates as Subordinates
import qualified Scrod.Unstable.Type.Warning as Warning

-- | A single entry in a module's export list.
-- Mirrors GHC's IE type but simplified.
data Export
  = -- | A named export: variable, type/class, or module re-export.
    -- Variables: @foo@, @pattern P@
    -- Types/classes with optional subordinates: @Foo@, @Foo(..)@, @Foo(Bar, Baz)@
    -- Module re-exports: @module Data.List@
    -- With optional warning: @{-# WARNING "msg" #-} foo@
    Identifier ExportName.ExportName (Maybe Subordinates.Subordinates) (Maybe Warning.Warning) (Maybe Doc.Doc)
  | -- | Section heading: @-- * Section@
    Group Section.Section
  | -- | Inline documentation: @-- | Some doc@
    Doc Doc.Doc
  | -- | Named doc reference: @-- $chunkName@
    DocNamed Text.Text
  deriving (Eq, Ord, Show)
