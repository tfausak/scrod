module Scrod.Type.Messages where

import qualified GHC.Utils.Error as ErrUtil
import qualified GHC.Utils.Outputable as Outputable

-- | Wrapper to avoid orphans.
newtype Messages a = Messages
  { unwrap :: ErrUtil.Messages a
  }

-- | Provided for convenience.
instance (ErrUtil.Diagnostic a) => Show (Messages a) where
  show = Outputable.showPprUnsafe . unwrap
