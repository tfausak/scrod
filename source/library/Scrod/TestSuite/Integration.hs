{-# LANGUAGE MultilineStrings #-}

module Scrod.TestSuite.Integration where

import qualified Control.Monad as Monad
import qualified GHC.Stack as Stack
import qualified Scrod.Convert.FromGhc as FromGhc
import qualified Scrod.Convert.ToJson as ToJson
import qualified Scrod.Extra.Parsec as Parsec
import qualified Scrod.Ghc.Parse as Parse
import qualified Scrod.Json.Value as Json
import qualified Scrod.JsonPointer.Evaluate as Pointer
import qualified Scrod.JsonPointer.Pointer as Pointer
import qualified Scrod.Spec as Spec

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = do
  Spec.describe s "integration" $ do
    Spec.it s "works with empty input" $ do
      check
        s
        ""
        [ ("/language", "null"),
          ("/extensions", "{}"),
          ("/documentation/type", "\"Empty\""),
          ("/documentation/value", "null"),
          ("/since", "null"),
          ("/name", "null"),
          ("/warning", "null"),
          ("/exports", "null"),
          ("/items", "[]")
        ]

    Spec.describe s "name" $ do
      Spec.it s "gets the module name" $ do
        check
          s
          """
          module M where
          """
          [ ("/name/location/line", "1"),
            ("/name/location/column", "8"),
            ("/name/value", "\"M\"")
          ]

check :: (Stack.HasCallStack, Monad m) => Spec.Spec m n -> String -> [(String, String)] -> m ()
check s input assertions = do
  parsed <- either (Spec.assertFailure s) pure $ Parse.parse input
  module_ <- either (Spec.assertFailure s) pure $ FromGhc.fromGhc parsed
  let json = ToJson.toJson module_
  Monad.forM_ assertions $ \(p, j) -> do
    pointer <- maybe (Spec.assertFailure s "invalid pointer") pure $ Parsec.parseString Pointer.decode p
    maybeJson <-
      if null j
        then pure Nothing
        else maybe (Spec.assertFailure s "invalid json") (pure . Just) $ Parsec.parseString Json.decode j
    Spec.assertEq s (Pointer.evaluate pointer json) maybeJson
