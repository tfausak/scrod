module Scrod.Unstable.Type.Export where

import qualified Data.Text as Text
import qualified Scrod.Unstable.Type.Doc as Doc
import qualified Scrod.Unstable.Type.ExportName as ExportName
import qualified Scrod.Unstable.Type.Level as Level
import qualified Scrod.Unstable.Type.ModuleName as ModuleName
import qualified Scrod.Unstable.Type.Subordinates as Subordinates
import qualified Scrod.Unstable.Type.Warning as Warning

-- | A single entry in a module's export list.
-- Mirrors GHC's IE type but simplified.
data Export
  = -- | A variable: @foo@
    -- With optional warning: @{-# WARNING "msg" #-} foo@
    Var ExportName.ExportName (Maybe Warning.Warning) (Maybe Doc.Doc)
  | -- | A type or class, with optional subordinates:
    -- @Foo@, @Foo(..)@, @Foo(Bar, Baz)@, or @Foo(Bar, ..)@
    -- With optional warning: @{-# WARNING "msg" #-} Foo(..)@
    Thing ExportName.ExportName (Maybe Subordinates.Subordinates) (Maybe Warning.Warning) (Maybe Doc.Doc)
  | -- | Module re-export: @module Data.List@
    -- With optional warning: @{-# WARNING "msg" #-} module Data.List@
    Module ModuleName.ModuleName (Maybe Warning.Warning)
  | -- | Section heading: @-- * Section@
    Group Level.Level Doc.Doc
  | -- | Inline documentation: @-- | Some doc@
    Doc Doc.Doc
  | -- | Named doc reference: @-- $chunkName@
    DocNamed Text.Text
  deriving (Eq, Ord, Show)
