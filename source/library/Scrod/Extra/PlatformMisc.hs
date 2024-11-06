module Scrod.Extra.PlatformMisc where

import qualified GHC.Driver.Session as Session

emptyPlatformMisc :: Session.PlatformMisc
emptyPlatformMisc =
  Session.PlatformMisc
    { Session.platformMisc_ghcWithInterpreter = error "PlatformMisc.ghcWithInterpreter",
      Session.platformMisc_libFFI = error "PlatformMisc.libFFI",
      Session.platformMisc_llvmTarget = error "PlatformMisc.llvmTarget",
      Session.platformMisc_targetPlatformString = error "PlatformMisc.targetPlatformString"
    }
