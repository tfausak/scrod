-- | CLI entry point. Reads Haskell source from stdin, parses it via the
-- GHC API, converts to Scrod's core representation, and renders as JSON
-- to stdout.
module Scrod.Executable.Main where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.Except as ExceptT
import qualified Data.Bifunctor as Bifunctor
import qualified Data.ByteString.Builder as Builder
import qualified Data.Version as Version
import qualified GHC.Stack as Stack
import qualified PackageInfo_scrod as PackageInfo
import qualified Scrod.Convert.FromGhc as FromGhc
import qualified Scrod.Convert.ToJsonSchema as ToJsonSchema
import qualified Scrod.Executable.Config as Config
import qualified Scrod.Executable.Flag as Flag
import qualified Scrod.Extra.Builder as Builder
import qualified Scrod.Extra.Either as Either
import qualified Scrod.Extra.Semigroup as Semigroup
import qualified Scrod.Ghc.Parse as Parse
import qualified Scrod.Json.ToJson as ToJson
import qualified Scrod.Json.Value as Json
import qualified Scrod.Unlit as Unlit
import qualified Scrod.Version as Version
import qualified System.Console.GetOpt as GetOpt
import qualified System.Environment as Environment
import qualified System.IO as IO

defaultMain :: (Stack.HasCallStack) => IO ()
defaultMain = do
  name <- Environment.getProgName
  arguments <- Environment.getArgs
  result <- mainWith name arguments getContents
  either putStr (Builder.hPutBuilder IO.stdout) result

mainWith ::
  (Stack.HasCallStack, Exception.MonadThrow m) =>
  String ->
  [String] ->
  m String ->
  m (Either String Builder.Builder)
mainWith name arguments myGetContents = ExceptT.runExceptT $ do
  flags <- Trans.lift $ Flag.fromArguments arguments
  config <- Trans.lift $ Config.fromFlags flags
  let version = Version.showVersion Version.version
  Monad.when (Config.help config) $ do
    let header =
          unlines
            [ unwords [name, "version", version],
              Semigroup.around "<" ">" PackageInfo.homepage
            ]
    ExceptT.throwE $ GetOpt.usageInfo header Flag.optDescrs
  Monad.when (Config.version config) . ExceptT.throwE $ version <> "\n"
  Monad.when (Config.schema config) . ExceptT.throwE $
    Builder.toString (Json.encode ToJsonSchema.toJsonSchema) <> "\n"
  contents <- Trans.lift myGetContents
  source <-
    if Config.literate config
      then Either.throw . Bifunctor.first userError $ Unlit.unlit contents
      else pure contents
  let isSignature = Config.signature config
  result <- Either.throw . Bifunctor.first userError $ Parse.parse isSignature source
  module_ <- Either.throw . Bifunctor.first userError $ FromGhc.fromGhc isSignature result
  pure $ (Json.encode . ToJson.toJson) module_ <> Builder.charUtf8 '\n'
