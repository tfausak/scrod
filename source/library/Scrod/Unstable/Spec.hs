{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Scrod.Unstable.Spec where

import qualified Data.Either as Either
import qualified Data.Map as Map
import Heck (Test, assertEq, describe, it)
import Scrod.Unstable.Extra.Heck (assertSatisfies, expectRight)
import qualified Scrod.Unstable.Main as Main
import qualified Scrod.Unstable.Type.Column as Column
import qualified Scrod.Unstable.Type.Extension as Extension
import qualified Scrod.Unstable.Type.Interface as Interface
import qualified Scrod.Unstable.Type.Language as Language
import qualified Scrod.Unstable.Type.Line as Line
import qualified Scrod.Unstable.Type.Located as Located
import qualified Scrod.Unstable.Type.Location as Location
import qualified Scrod.Unstable.Type.ModuleName as ModuleName

spec :: (Monad m, Monad n) => Test m n -> n ()
spec t = t.describe "extract" $ do
  let f = expectRight t . Main.extract . unlines

  t.it "fails when there is a parse error" $ do
    assertSatisfies t Either.isLeft $ Main.extract "!"

  t.it "fails when there is an invalid language extension" $ do
    assertSatisfies t Either.isLeft $ Main.extract "{-# language x #-}"

  t.it "succeeds when the input is empty" $ do
    assertSatisfies t Either.isRight $ Main.extract ""

  t.describe "language" $ do
    t.it "has no language by default" $ do
      interface <- f []
      assertEq t interface.language Nothing

    t.it "handles language pragma" $ do
      interface <- f ["{-# language Haskell98 #-}"]
      assertEq t (fmap (.value) interface.language) $ Just "Haskell98"

    t.it "succeeds with Haskell2010" $ do
      interface <- f ["{-# language Haskell2010 #-}"]
      assertEq t (fmap (.value) interface.language) $ Just "Haskell2010"

    t.it "succeeds with GHC2021" $ do
      interface <- f ["{-# language GHC2021 #-}"]
      assertEq t (fmap (.value) interface.language) $ Just "GHC2021"

    t.it "succeeds with GHC2024" $ do
      interface <- f ["{-# language GHC2024 #-}"]
      assertEq t (fmap (.value) interface.language) $ Just "GHC2024"

    t.it "handles options_ghc pragma" $ do
      interface <- f ["{-# options_ghc -XHaskell98 #-}"]
      assertEq t (fmap (.value) interface.language) $ Just "Haskell98"

    t.it "handles options pragma" $ do
      interface <- f ["{-# options -XHaskell98 #-}"]
      assertEq t (fmap (.value) interface.language) $ Just "Haskell98"

    t.it "picks the last one in a pragma" $ do
      interface <- f ["{-# language Haskell98, Haskell2010 #-}"]
      assertEq t (fmap (.value) interface.language) $ Just "Haskell2010"

    t.it "picks the last pragma" $ do
      interface <- f ["{-# language Haskell98 #-}", "{-# language Haskell2010 #-}"]
      assertEq t (fmap (.value) interface.language) $ Just "Haskell2010"

  t.describe "extensions" $ do
    t.it "has no extensions by default" $ do
      interface <- f []
      assertEq t interface.extensions Map.empty

    t.it "handles language pragma" $ do
      interface <- f ["{-# language Arrows #-}"]
      assertEq t (Map.mapKeys (.value) interface.extensions) $ Map.singleton "Arrows" True

    t.it "handles options_ghc pragma" $ do
      interface <- f ["{-# options_ghc -XArrows #-}"]
      assertEq t (Map.mapKeys (.value) interface.extensions) $ Map.singleton "Arrows" True

    t.it "handles options pragma" $ do
      interface <- f ["{-# options -XArrows #-}"]
      assertEq t (Map.mapKeys (.value) interface.extensions) $ Map.singleton "Arrows" True

    t.it "handles disabling an extension" $ do
      interface <- f ["{-# language NoArrows #-}"]
      assertEq t (Map.mapKeys (.value) interface.extensions) $ Map.singleton "Arrows" False

    t.it "picks the last one in a pragma" $ do
      interface <- f ["{-# language Arrows, NoArrows #-}"]
      assertEq t (Map.mapKeys (.value) interface.extensions) $ Map.singleton "Arrows" False

    t.it "picks the last pragma" $ do
      interface <- f ["{-# language Arrows #-}", "{-# language NoArrows #-}"]
      assertEq t (Map.mapKeys (.value) interface.extensions) $ Map.singleton "Arrows" False

    t.it "handles unnecessarily enabling an extension" $ do
      interface <- f ["{-# language Haskell98, StarIsType #-}"]
      assertEq t (Map.mapKeys (.value) interface.extensions) $ Map.singleton "StarIsType" True

    t.it "handles disabling an extension enabled by the language" $ do
      interface <- f ["{-# language Haskell98, NoStarIsType #-}"]
      assertEq t (Map.mapKeys (.value) interface.extensions) $ Map.singleton "StarIsType" False

    t.it "handles implied extensions" $ do
      interface <- f ["{-# language PolyKinds #-}"]
      assertEq t (Map.mapKeys (.value) interface.extensions) $
        Map.fromList
          [ ("KindSignatures", True),
            ("PolyKinds", True)
          ]

    t.it "supports Glasgow extensions" $ do
      interface <- f ["{-# options_ghc -fglasgow-exts #-}"]
      assertEq t (Map.mapKeys (.value) interface.extensions) $
        Map.fromList
          [ ("ConstrainedClassMethods", True),
            ("DeriveDataTypeable", True),
            ("DeriveFoldable", True),
            ("DeriveFunctor", True),
            ("DeriveGeneric", True),
            ("DeriveTraversable", True),
            ("EmptyDataDecls", True),
            ("ExistentialQuantification", True),
            ("ExplicitForAll", True),
            ("ExplicitNamespaces", True),
            ("FlexibleContexts", True),
            ("FlexibleInstances", True),
            ("ForeignFunctionInterface", True),
            ("FunctionalDependencies", True),
            ("GeneralizedNewtypeDeriving", True),
            ("ImplicitParams", True),
            ("KindSignatures", True),
            ("LiberalTypeSynonyms", True),
            ("MagicHash", True),
            ("MultiParamTypeClasses", True),
            ("ParallelListComp", True),
            ("PatternGuards", True),
            ("PostfixOperators", True),
            ("RankNTypes", True),
            ("RecursiveDo", True),
            ("ScopedTypeVariables", True),
            ("StandaloneDeriving", True),
            ("TypeOperators", True),
            ("TypeSynonymInstances", True),
            ("UnboxedSums", True),
            ("UnboxedTuples", True),
            ("UnicodeSyntax", True),
            ("UnliftedFFITypes", True)
          ]

  t.describe "moduleName" $ do
    t.it "has no module name by default" $ do
      interface <- f []
      assertEq t interface.moduleName Nothing

    t.it "gets the name" $ do
      interface <- f ["module M where"]
      assertEq t (fmap (.value.value) interface.moduleName) $ Just "M"

    t.it "gets the line" $ do
      interface <- f ["module M where"]
      assertEq t (fmap (.location.line.value) interface.moduleName) $ Just 1

    t.it "gets the column" $ do
      interface <- f ["module M where"]
      assertEq t (fmap (.location.column.value) interface.moduleName) $ Just 8
