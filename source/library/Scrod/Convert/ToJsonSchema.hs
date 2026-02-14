{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Generate a JSON Schema describing the output of 'ToJson.toJson'.
--
-- Produces a JSON Schema (2020-12) as a 'Json.Value'. The root schema is
-- generated from the 'ToSchema.ToSchema' type class instance for
-- 'Module.Module', with root-level metadata and accumulated @$defs@
-- prepended and appended.
module Scrod.Convert.ToJsonSchema where

import qualified Data.Proxy as Proxy
import qualified Scrod.Core.Module as Module
import qualified Scrod.Extra.Builder as Builder
import qualified Scrod.Extra.Parsec as Parsec
import qualified Scrod.Json.Object as Object
import qualified Scrod.Json.Value as Json
import qualified Scrod.JsonPointer.Evaluate as Pointer
import qualified Scrod.JsonPointer.Pointer as Pointer
import qualified Scrod.Schema as ToSchema
import qualified Scrod.Spec as Spec

-- | Schema for the top-level module object.
--
-- Runs the 'ToSchema.ToSchema' instance for 'Module.Module' in
-- 'ToSchema.SchemaM', extracts the resulting object schema, and wraps
-- it with JSON Schema metadata (@$schema@, @$id@, @title@,
-- @description@) and any accumulated @$defs@.
toJsonSchema :: Json.Value
toJsonSchema =
  let (schema, defs) =
        ToSchema.runSchemaM $
          ToSchema.toSchema
            (Proxy.Proxy :: Proxy.Proxy Module.Module)
   in case ToSchema.unwrap schema of
        Json.Object (Object.MkObject bodyPairs) ->
          Json.Object . Object.MkObject $
            [ Json.pair "$schema" (Json.string "https://json-schema.org/draft/2020-12/schema"),
              Json.pair "$id" (Json.string "https://scrod.fyi/schema.json"),
              Json.pair "title" (Json.string "Scrod"),
              Json.pair "description" (Json.string "JSON output of the Scrod Haskell documentation tool.")
            ]
              <> bodyPairs
              <> [Json.pair "$defs" (Json.object defs)]
        _ -> Json.null

-- * Tests

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = do
  Spec.named s 'toJsonSchema $ do
    Spec.it s "round-trips through JSON encode/decode" $ do
      let encoded = Builder.toString $ Json.encode toJsonSchema
      Spec.assertNe s (Parsec.parseString Json.decode encoded) Nothing

    Spec.it s "has the expected $schema" $ do
      at s "/$schema" $ Json.string "https://json-schema.org/draft/2020-12/schema"

    Spec.it s "has the expected title" $ do
      at s "/title" $ Json.string "Scrod"

    Spec.it s "has type object" $ do
      at s "/type" $ Json.string "object"

    Spec.it s "defines doc" $ do
      at s "/$defs/doc/oneOf" Json.null

    Spec.it s "has version property" $ do
      at s "/properties/version/type" $ Json.string "array"

    Spec.it s "has items property" $ do
      at s "/properties/items/type" $ Json.string "array"

-- | Assert that a JSON Pointer path resolves to the expected value in the
-- schema.
at :: (Applicative m) => Spec.Spec m n -> String -> Json.Value -> m ()
at s path expected = do
  let json = toJsonSchema
  case Parsec.parseString Pointer.decode path of
    Nothing -> Spec.assertFailure s $ "invalid pointer: " <> path
    Just pointer -> case Pointer.evaluate pointer json of
      Nothing -> Spec.assertFailure s $ "path not found: " <> path
      Just actual ->
        -- For the $defs check where we just want to confirm existence, we
        -- pass null as expected and skip the equality check.
        if expected == Json.null
          then pure ()
          else Spec.assertEq s actual expected
