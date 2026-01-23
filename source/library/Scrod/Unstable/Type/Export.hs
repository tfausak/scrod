module Scrod.Unstable.Type.Export where

import qualified Data.Text as Text
import qualified Scrod.Unstable.Type.Doc as Doc
import qualified Scrod.Unstable.Type.ExportName as ExportName
import qualified Scrod.Unstable.Type.Level as Level
import qualified Scrod.Unstable.Type.ModuleName as ModuleName
import qualified Scrod.Unstable.Type.Subordinates as Subordinates

-- | A single entry in a module's export list.
-- Mirrors GHC's IE type but simplified.
data Export
  = -- | A variable: @foo@
    Var ExportName.ExportName (Maybe Doc.Doc)
  | -- | A type or class, with optional subordinates:
    -- @Foo@, @Foo(..)@, @Foo(Bar, Baz)@, or @Foo(Bar, ..)@
    Thing ExportName.ExportName (Maybe Subordinates.Subordinates) (Maybe Doc.Doc)
  | -- | Module re-export: @module Data.List@
    Module ModuleName.ModuleName
  | -- | Section heading: @-- * Section@
    Group Level.Level Doc.Doc
  | -- | Inline documentation: @-- | Some doc@
    Doc Doc.Doc
  | -- | Named doc reference: @-- $chunkName@
    DocNamed Text.Text
  deriving (Eq, Ord, Show)
