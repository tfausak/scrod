-- | Convert the GHC module export list into Scrod's 'Export.Export' type.
--
-- Handles all 'IE' (import\/export) variants: identifiers (with optional
-- subordinates and wildcards), module re-exports, section groups,
-- documentation comments, and named documentation chunks.
module Scrod.Convert.FromGhc.Exports where

import qualified Data.Text as Text
import qualified GHC.Hs.Extension as Ghc
import qualified GHC.Hs.ImpExp as ImpExp
import qualified GHC.Types.SrcLoc as SrcLoc
import qualified GHC.Unit.Module.Warnings as Warnings
import qualified Language.Haskell.Syntax as Syntax
import qualified Scrod.Convert.FromGhc.Doc as GhcDoc
import qualified Scrod.Convert.FromGhc.Internal as Internal
import qualified Scrod.Core.Export as Export
import qualified Scrod.Core.ExportIdentifier as ExportIdentifier
import qualified Scrod.Core.ExportName as ExportName
import qualified Scrod.Core.ExportNameKind as ExportNameKind
import qualified Scrod.Core.Header as Header
import qualified Scrod.Core.Level as Level
import qualified Scrod.Core.ModuleName as ModuleName
import qualified Scrod.Core.Section as Section
import qualified Scrod.Core.Subordinates as Subordinates
import qualified Scrod.Core.Warning as Warning

-- | Extract module export list.
extractModuleExports ::
  SrcLoc.Located (Syntax.HsModule Ghc.GhcPs) ->
  Maybe [Export.Export]
extractModuleExports lHsModule = do
  let hsModule = SrcLoc.unLoc lHsModule
  lExports <- Syntax.hsmodExports hsModule
  let exports = SrcLoc.unLoc lExports
  Just $ fmap convertIE exports

-- | Convert an IE (import/export) entry to our 'Export' type.
convertIE ::
  SrcLoc.GenLocated l (Syntax.IE Ghc.GhcPs) ->
  Export.Export
convertIE lIe = case SrcLoc.unLoc lIe of
  Syntax.IEVar mLWarning lName mDoc ->
    Export.Identifier
      ExportIdentifier.MkExportIdentifier
        { ExportIdentifier.name = convertWrappedName lName,
          ExportIdentifier.subordinates = Nothing,
          ExportIdentifier.warning = convertExportWarning mLWarning,
          ExportIdentifier.doc = GhcDoc.convertExportDoc <$> mDoc
        }
  Syntax.IEThingAbs mLWarning lName mDoc ->
    Export.Identifier
      ExportIdentifier.MkExportIdentifier
        { ExportIdentifier.name = convertWrappedName lName,
          ExportIdentifier.subordinates = Nothing,
          ExportIdentifier.warning = convertExportWarning mLWarning,
          ExportIdentifier.doc = GhcDoc.convertExportDoc <$> mDoc
        }
  Syntax.IEThingAll (mLWarning, _) lName mDoc ->
    Export.Identifier
      ExportIdentifier.MkExportIdentifier
        { ExportIdentifier.name = convertWrappedName lName,
          ExportIdentifier.subordinates =
            Just
              Subordinates.MkSubordinates
                { Subordinates.wildcard = True,
                  Subordinates.explicit = []
                },
          ExportIdentifier.warning = convertExportWarning mLWarning,
          ExportIdentifier.doc = GhcDoc.convertExportDoc <$> mDoc
        }
  Syntax.IEThingWith (mLWarning, _) lName wildcard children mDoc ->
    Export.Identifier
      ExportIdentifier.MkExportIdentifier
        { ExportIdentifier.name = convertWrappedName lName,
          ExportIdentifier.subordinates =
            Just
              Subordinates.MkSubordinates
                { Subordinates.wildcard = hasWildcard wildcard,
                  Subordinates.explicit = fmap convertWrappedName children
                },
          ExportIdentifier.warning = convertExportWarning mLWarning,
          ExportIdentifier.doc = GhcDoc.convertExportDoc <$> mDoc
        }
  Syntax.IEModuleContents (mLWarning, _) lModName ->
    Export.Identifier
      ExportIdentifier.MkExportIdentifier
        { ExportIdentifier.name =
            ExportName.MkExportName
              { ExportName.kind = Just ExportNameKind.Module,
                ExportName.name = ModuleName.unwrap . Internal.moduleNameFromGhc $ SrcLoc.unLoc lModName
              },
          ExportIdentifier.subordinates = Nothing,
          ExportIdentifier.warning = convertExportWarning mLWarning,
          ExportIdentifier.doc = Nothing
        }
  Syntax.IEGroup _ level lDoc ->
    Export.Group
      Section.MkSection
        { Section.header =
            Header.MkHeader
              { Header.level = Level.fromInt level,
                Header.title = GhcDoc.convertExportDoc lDoc
              }
        }
  Syntax.IEDoc _ lDoc ->
    Export.Doc $ GhcDoc.convertExportDoc lDoc
  Syntax.IEDocNamed _ name ->
    Export.DocNamed $ Text.pack name

-- | Check if an IE wildcard is present.
hasWildcard :: ImpExp.IEWildcard -> Bool
hasWildcard wildcard = case wildcard of
  ImpExp.NoIEWildcard -> False
  ImpExp.IEWildcard _ -> True

-- | Convert export warning.
convertExportWarning ::
  Maybe (SrcLoc.GenLocated l (Warnings.WarningTxt Ghc.GhcPs)) ->
  Maybe Warning.Warning
convertExportWarning = fmap (Internal.warningTxtToWarning . SrcLoc.unLoc)

-- | Convert a wrapped name to our 'ExportName' type.
convertWrappedName ::
  SrcLoc.GenLocated l (ImpExp.IEWrappedName Ghc.GhcPs) ->
  ExportName.ExportName
convertWrappedName lWrapped = case SrcLoc.unLoc lWrapped of
  ImpExp.IEName _ lId ->
    ExportName.MkExportName
      { ExportName.kind = Nothing,
        ExportName.name = Internal.extractRdrName lId
      }
  ImpExp.IEPattern _ lId ->
    ExportName.MkExportName
      { ExportName.kind = Just ExportNameKind.Pattern,
        ExportName.name = Internal.extractRdrName lId
      }
  ImpExp.IEType _ lId ->
    ExportName.MkExportName
      { ExportName.kind = Just ExportNameKind.Type,
        ExportName.name = Internal.extractRdrName lId
      }
  ImpExp.IEDefault _ lId ->
    ExportName.MkExportName
      { ExportName.kind = Nothing,
        ExportName.name = Internal.extractRdrName lId
      }
  ImpExp.IEData _ lId ->
    ExportName.MkExportName
      { ExportName.kind = Nothing,
        ExportName.name = Internal.extractRdrName lId
      }
