module Scrod.Extra.GhcNameVersion where

import qualified GHC.Driver.Session as Session

emptyGhcNameVersion :: Session.GhcNameVersion
emptyGhcNameVersion =
  Session.GhcNameVersion
    { Session.ghcNameVersion_programName = error "GhcNameVersion.programName",
      Session.ghcNameVersion_projectVersion = error "GhcNameVersion.projectVersion"
    }
