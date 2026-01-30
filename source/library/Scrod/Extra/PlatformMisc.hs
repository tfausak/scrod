{-# LANGUAGE TemplateHaskellQuotes #-}

module Scrod.Extra.PlatformMisc where

import qualified GHC.Driver.Session as Session
import qualified Scrod.Exception.Uninitialized as Uninitialized

empty :: Session.PlatformMisc
empty =
  Session.PlatformMisc
    { Session.platformMisc_ghcWithInterpreter = Uninitialized.throw 'Session.platformMisc_ghcWithInterpreter,
      Session.platformMisc_libFFI = Uninitialized.throw 'Session.platformMisc_libFFI,
      Session.platformMisc_llvmTarget = Uninitialized.throw 'Session.platformMisc_llvmTarget,
      Session.platformMisc_targetPlatformString = Uninitialized.throw 'Session.platformMisc_targetPlatformString
    }
