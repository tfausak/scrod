module Scrod.Unstable.Convert where

import qualified Control.Exception as Exception
import qualified Data.Map as Map
-- import qualified Data.Semigroup as Semigroup
import qualified Data.Tuple as Tuple
import qualified GHC.Driver.DynFlags as DynFlags
import qualified GHC.Driver.Session as Session
import qualified GHC.Hs.Extension as Ghc
import qualified GHC.LanguageExtensions.Type as GhcExtension
import qualified GHC.Parser.Annotation as Annotation
import qualified GHC.Parser.Errors.Types as Errors
import qualified GHC.Types.Error as Error
import qualified GHC.Types.SourceError as SourceError
import qualified GHC.Types.SrcLoc as SrcLoc
import qualified GHC.Utils.Outputable as Outputable
import qualified Language.Haskell.Syntax as Hs
import qualified Scrod.Unstable.Extra.OnOff as OnOff
import qualified Scrod.Unstable.Type.Extension as Extension
import qualified Scrod.Unstable.Type.Interface as Interface
import qualified Scrod.Unstable.Type.Language as Language
import qualified Scrod.Unstable.Type.Located as Located
import qualified Scrod.Unstable.Type.ModuleName as ModuleName

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
          Interface.moduleName = extractModuleName lHsModule
        }

extensionsToMap ::
  [DynFlags.OnOff GhcExtension.Extension] ->
  Map.Map Extension.Extension Bool
extensionsToMap =
  Map.fromListWith (\_ x -> x)
    . fmap (Tuple.swap . fmap Extension.fromGhc . OnOff.onOff ((,) True) ((,) False))

extractModuleName ::
  SrcLoc.Located (Hs.HsModule Ghc.GhcPs) ->
  Maybe (Located.Located ModuleName.ModuleName)
extractModuleName lHsModule = do
  let hsModule = SrcLoc.unLoc lHsModule
  lModuleName <- Hs.hsmodName hsModule
  let srcSpan = Annotation.getLocA lModuleName
      moduleName = ModuleName.fromGhc $ SrcLoc.unLoc lModuleName
  Located.fromGhc $ SrcLoc.L srcSpan moduleName
