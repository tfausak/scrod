module LegendaryChainsaw.Ghc.Platform where

import qualified GHC.ByteOrder as ByteOrder
import qualified GHC.Platform as Platform
import qualified LegendaryChainsaw.Ghc.ArchOS as ArchOS

empty :: Platform.Platform
empty =
  Platform.Platform
    { Platform.platformArchOS = ArchOS.empty,
      Platform.platformWordSize = Platform.PW8,
      Platform.platformByteOrder = ByteOrder.LittleEndian,
      Platform.platformUnregisterised = False,
      Platform.platformHasGnuNonexecStack = False,
      Platform.platformHasIdentDirective = False,
      Platform.platformHasSubsectionsViaSymbols = False,
      Platform.platformIsCrossCompiling = False,
      Platform.platformLeadingUnderscore = False,
      Platform.platformTablesNextToCode = False,
      Platform.platformHasLibm = False,
      Platform.platform_constants = Nothing
    }
