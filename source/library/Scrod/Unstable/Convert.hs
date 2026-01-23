module Scrod.Unstable.Convert where

import qualified Control.Exception as Exception
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Tuple as Tuple
import qualified Documentation.Haddock.Parser as Haddock
import qualified Documentation.Haddock.Types as Haddock
import qualified GHC.Data.FastString as FastString
import qualified GHC.Driver.DynFlags as DynFlags
import qualified GHC.Driver.Session as Session
import qualified GHC.Hs as Hs
import qualified GHC.Hs.Doc as HsDoc
import qualified GHC.Hs.DocString as DocString
import qualified GHC.Hs.Extension as Ghc
import qualified GHC.LanguageExtensions.Type as GhcExtension
import qualified GHC.Parser.Annotation as Annotation
import qualified GHC.Parser.Errors.Types as Errors
import qualified GHC.Types.Error as Error
import qualified GHC.Types.SourceError as SourceError
import qualified GHC.Types.SourceText as SourceText
import qualified GHC.Types.SrcLoc as SrcLoc
import qualified GHC.Unit.Module.Warnings as Warnings
import qualified GHC.Utils.Outputable as Outputable
import qualified Language.Haskell.Syntax as Syntax
import qualified Scrod.Unstable.Extra.OnOff as OnOff
import qualified Scrod.Unstable.Type.Category as Category
import qualified Scrod.Unstable.Type.Doc as Doc
import qualified Scrod.Unstable.Type.Doc.Convert as DocConvert
import qualified Scrod.Unstable.Type.Extension as Extension
import qualified Scrod.Unstable.Type.Interface as Interface
import qualified Scrod.Unstable.Type.Language as Language
import qualified Scrod.Unstable.Type.Located as Located
import qualified Scrod.Unstable.Type.ModuleName as ModuleName
import qualified Scrod.Unstable.Type.PackageName as PackageName
import qualified Scrod.Unstable.Type.Since as Since
import qualified Scrod.Unstable.Type.Version as Version
import qualified Scrod.Unstable.Type.Warning as Warning

convert ::
  Either
    (Either SourceError.SourceError (Error.Messages Errors.PsMessage))
    ( (Maybe Session.Language, [DynFlags.OnOff GhcExtension.Extension]),
      SrcLoc.Located (Hs.HsModule Ghc.GhcPs)
    ) ->
  Either String Interface.Interface
convert input = case input of
  Left (Left sourceError) ->
    Left $ Exception.displayException sourceError
  Left (Right messages) ->
    Left . Outputable.showSDocUnsafe $ Outputable.ppr messages
  Right ((language, extensions), lHsModule) ->
    Right
      Interface.MkInterface
        { Interface.language = fmap Language.fromGhc language,
          Interface.extensions = extensionsToMap extensions,
          Interface.documentation = extractModuleDocumentation lHsModule,
          Interface.name = extractModuleName lHsModule,
          Interface.since = extractModuleSince lHsModule,
          Interface.warning = extractModuleWarning lHsModule
        }

extensionsToMap ::
  [DynFlags.OnOff GhcExtension.Extension] ->
  Map.Map Extension.Extension Bool
extensionsToMap =
  Map.fromListWith (\_ x -> x)
    . fmap (Tuple.swap . fmap Extension.fromGhc . OnOff.onOff ((,) True) ((,) False))

extractModuleName ::
  SrcLoc.Located (Syntax.HsModule Ghc.GhcPs) ->
  Maybe (Located.Located ModuleName.ModuleName)
extractModuleName lHsModule = do
  let hsModule = SrcLoc.unLoc lHsModule
  lModuleName <- Syntax.hsmodName hsModule
  let srcSpan = Annotation.getLocA lModuleName
      moduleName = ModuleName.fromGhc $ SrcLoc.unLoc lModuleName
  Located.fromGhc $ SrcLoc.L srcSpan moduleName

extractModuleDocumentation ::
  SrcLoc.Located (Syntax.HsModule Ghc.GhcPs) ->
  Doc.Doc
extractModuleDocumentation =
  maybe Doc.Empty DocConvert.parseDoc
    . extractRawDocString

extractRawDocString ::
  SrcLoc.Located (Syntax.HsModule Ghc.GhcPs) ->
  Maybe String
extractRawDocString lHsModule = do
  let hsModule = SrcLoc.unLoc lHsModule
      xModulePs = Syntax.hsmodExt hsModule
  lHsDoc <- Hs.hsmodHaddockModHeader xModulePs
  let hsDoc = SrcLoc.unLoc lHsDoc
      hsDocString = HsDoc.hsDocString hsDoc
  pure $ DocString.renderHsDocString hsDocString

extractModuleSince ::
  SrcLoc.Located (Syntax.HsModule Ghc.GhcPs) ->
  Since.Since
extractModuleSince lHsModule =
  case extractModuleMeta lHsModule of
    Nothing -> Since.empty
    Just meta ->
      Since.MkSince
        { Since.package = fmap PackageName.fromString $ Haddock._package meta,
          Since.version = Version.fromHaddock =<< Haddock._version meta
        }

extractModuleMeta ::
  SrcLoc.Located (Syntax.HsModule Ghc.GhcPs) ->
  Maybe Haddock.Meta
extractModuleMeta =
  fmap Haddock._meta
    . extractModuleMetaDoc

extractModuleMetaDoc ::
  SrcLoc.Located (Syntax.HsModule Ghc.GhcPs) ->
  Maybe (Haddock.MetaDoc m Haddock.Identifier)
extractModuleMetaDoc lHsModule = do
  rendered <- extractRawDocString lHsModule
  pure $ Haddock.parseParas Nothing rendered

extractModuleWarning ::
  SrcLoc.Located (Syntax.HsModule Ghc.GhcPs) ->
  Maybe Warning.Warning
extractModuleWarning lHsModule = do
  let hsModule = SrcLoc.unLoc lHsModule
      xModulePs = Syntax.hsmodExt hsModule
  lWarningTxt <- Hs.hsmodDeprecMessage xModulePs
  let warningTxt = SrcLoc.unLoc lWarningTxt
  pure $ warningTxtToWarning warningTxt

warningTxtToWarning :: Warnings.WarningTxt Ghc.GhcPs -> Warning.Warning
warningTxtToWarning warningTxt =
  Warning.MkWarning
    { Warning.category = Category.fromGhc $ Warnings.warningTxtCategory warningTxt,
      Warning.value = Text.intercalate (Text.singleton '\n') . fmap extractMessage $ Warnings.warningTxtMessage warningTxt
    }

extractMessage :: SrcLoc.GenLocated l (HsDoc.WithHsDocIdentifiers SourceText.StringLiteral Ghc.GhcPs) -> Text.Text
extractMessage =
  Text.pack
    . FastString.unpackFS
    . SourceText.sl_fs
    . HsDoc.hsDocString
    . SrcLoc.unLoc
