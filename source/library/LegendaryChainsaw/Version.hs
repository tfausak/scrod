{-# LANGUAGE TemplateHaskellQuotes #-}

module LegendaryChainsaw.Version where

import qualified Data.Version as Version
import qualified LegendaryChainsaw.Spec as Spec
import qualified PackageInfo_legendary_chainsaw as PackageInfo

version :: Version.Version
version = PackageInfo.version

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = do
  Spec.named s 'version $ do
    Spec.it s "has a non-empty branch" $ do
      Spec.assertEq s (null $ Version.versionBranch version) False

    Spec.it s "has no tags" $ do
      -- Can't use `Version.versionTags` because it's deprecated.
      let (Version.Version _ tags) = version
      Spec.assertEq s (null tags) True
