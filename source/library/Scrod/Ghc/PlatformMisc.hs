{-# LANGUAGE TemplateHaskellQuotes #-}

module Scrod.Ghc.PlatformMisc where

import qualified GHC.Driver.Session as Session
import qualified GHC.Stack as Stack
import qualified Scrod.Ghc.Uninitialized as Uninitialized

empty :: (Stack.HasCallStack) => Session.PlatformMisc
empty =
  Session.PlatformMisc
    { Session.platformMisc_ghcWithInterpreter = Uninitialized.throw 'Session.platformMisc_ghcWithInterpreter,
      Session.platformMisc_libFFI = Uninitialized.throw 'Session.platformMisc_libFFI,
      Session.platformMisc_llvmTarget = Uninitialized.throw 'Session.platformMisc_llvmTarget,
      Session.platformMisc_targetPlatformString = Uninitialized.throw 'Session.platformMisc_targetPlatformString,
      Session.platformMisc_targetRTSLinkerOnlySupportsSharedLibs = Uninitialized.throw 'Session.platformMisc_targetRTSLinkerOnlySupportsSharedLibs
    }
