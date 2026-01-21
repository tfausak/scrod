{-# LANGUAGE TemplateHaskellQuotes #-}

module Scrod.Unstable.Extra.GhcNameVersion where

import qualified GHC.Driver.Session as Session
import qualified Scrod.Unstable.Exception.Uninitialized as Uninitialized

empty :: Session.GhcNameVersion
empty =
  Session.GhcNameVersion
    { Session.ghcNameVersion_programName = Uninitialized.throw 'Session.ghcNameVersion_programName,
      Session.ghcNameVersion_projectVersion = Uninitialized.throw 'Session.ghcNameVersion_projectVersion
    }
