module LegendaryChainsaw.Ghc.UnitSettings where

import qualified GHC.Settings as Settings
import qualified LegendaryChainsaw.Ghc.UnitId as UnitId

empty :: Settings.UnitSettings
empty = Settings.UnitSettings {Settings.unitSettings_baseUnitId = UnitId.empty}
