module LegendaryChainsaw.Core.Since where

import qualified LegendaryChainsaw.Core.PackageName as PackageName
import qualified LegendaryChainsaw.Core.Version as Version

data Since = MkSince
  { package :: Maybe PackageName.PackageName,
    version :: Version.Version
  }
  deriving (Eq, Ord, Show)
