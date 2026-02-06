module Scrod.Ghc.ArchOS where

import qualified GHC.Platform.ArchOS as ArchOS

empty :: ArchOS.ArchOS
empty =
  ArchOS.ArchOS
    { ArchOS.archOS_arch = ArchOS.ArchUnknown,
      ArchOS.archOS_OS = ArchOS.OSUnknown
    }
