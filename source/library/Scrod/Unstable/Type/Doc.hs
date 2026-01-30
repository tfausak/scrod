{-# LANGUAGE DeriveGeneric #-}

module Scrod.Unstable.Type.Doc where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified GHC.Generics as Generics
import qualified Scrod.Unstable.Type.Example as Example
import qualified Scrod.Unstable.Type.Header as Header
import qualified Scrod.Unstable.Type.Hyperlink as Hyperlink
import qualified Scrod.Unstable.Type.Identifier as Identifier
import qualified Scrod.Unstable.Type.JsonOptions as JsonOptions
import qualified Scrod.Unstable.Type.ModLink as ModLink
import qualified Scrod.Unstable.Type.Picture as Picture
import qualified Scrod.Unstable.Type.Table as Table

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
  deriving (Eq, Ord, Show, Generics.Generic)

instance Semigroup Doc where
  x <> y = case (x, y) of
    (Empty, _) -> y
    (_, Empty) -> x
    _ -> Append x y

instance Monoid Doc where
  mempty = Empty

instance Aeson.FromJSON Doc where
  parseJSON = Aeson.genericParseJSON JsonOptions.sumOptions

instance Aeson.ToJSON Doc where
  toJSON = Aeson.genericToJSON JsonOptions.sumOptions
