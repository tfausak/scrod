{-# LANGUAGE FlexibleInstances #-}

module Scrod.Type.LHsModule where

import qualified GHC.Hs
import qualified GHC.Types.SrcLoc as SrcLoc
import qualified Language.Haskell.Syntax as HS
import qualified Scrod.Extra.Data as Data

-- | Wrapper to avoid orphans.
newtype LHsModule a = LHsModule
  { unwrap :: SrcLoc.Located (HS.HsModule a)
  }

-- | Provided for convenience.
instance Show (LHsModule GHC.Hs.GhcPs) where
  show = flip (Data.showS . unwrap) ""
