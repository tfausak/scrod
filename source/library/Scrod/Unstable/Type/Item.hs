module Scrod.Unstable.Type.Item where

import qualified Scrod.Unstable.Type.Doc as Doc

newtype Item = MkItem
  { documentation :: Doc.Doc
  }
  deriving (Eq, Ord, Show)
