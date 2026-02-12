{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Type class for generating JSON Schema descriptions of types.
--
-- This parallels 'Scrod.Json.ToJson.ToJson' but operates on types rather
-- than values: each instance describes the JSON Schema for the type's
-- 'ToJson' encoding. An 'Control.Monad.Trans.Accum.Accum' monad
-- accumulates named definitions (for @$defs@).
module Scrod.Schema where

import qualified Control.Monad.Trans.Accum as Accum
import qualified Data.Kind as Kind
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Proxy as Proxy
import qualified Data.Text as Text
import qualified GHC.Generics as Generics
import qualified GHC.TypeLits as TypeLits
import qualified Numeric.Natural as Natural
import qualified Scrod.Json.Value as Json
import qualified Scrod.Spec as Spec

-- | A JSON Schema value.
newtype Schema = MkSchema
  { unwrap :: Json.Value
  }
  deriving (Eq, Ord, Show)

-- | Monad for schema generation. Accumulates named schema definitions
-- (keyed by name) that become entries in the @$defs@ section of a root
-- schema. Using a 'Map.Map' ensures each definition is stored once even
-- if registered multiple times.
type SchemaM = Accum.Accum (Map.Map String Json.Value)

-- | Run a schema computation, returning the result and accumulated
-- definitions as an association list sorted by name.
runSchemaM :: SchemaM a -> (a, [(String, Json.Value)])
runSchemaM m =
  let (a, defs) = Accum.runAccum m Map.empty
   in (a, Map.toAscList defs)

-- | Register a named schema definition and return a @$ref@ pointing to
-- it. The definition is added to the accumulated @$defs@. Characters
-- @~@ and @/@ in the name are escaped per RFC 6901 for the JSON Pointer
-- in the @$ref@.
define :: String -> SchemaM Schema -> SchemaM Schema
define name m = do
  MkSchema s <- m
  Accum.add $ Map.singleton name s
  pure . MkSchema $ Json.object [("$ref", Json.string $ "#/$defs/" <> escapeJsonPointer name)]

-- | Escape a JSON Pointer reference token per RFC 6901: @~@ becomes
-- @~0@ and @/@ becomes @~1@.
escapeJsonPointer :: String -> String
escapeJsonPointer = List.concatMap $ \c -> case c of
  '~' -> "~0"
  '/' -> "~1"
  _ -> [c]

-- | Convert a type to its JSON Schema representation.
--
-- Use @deriving via 'Generics.Generically'@ with a 'Generics.Generic'
-- instance to derive 'ToSchema' for record types, enum types, and tagged
-- sum types.
type ToSchema :: Kind.Type -> Kind.Constraint
class ToSchema a where
  toSchema :: Proxy.Proxy a -> SchemaM Schema

  -- | Whether the type represents an optional (omit-when-absent) field.
  -- Returns 'True' for 'Maybe', 'False' for everything else. Used by
  -- the generic record schema to separate required from optional
  -- properties.
  isOptional :: Proxy.Proxy a -> Bool
  isOptional _ = False

instance ToSchema Bool where
  toSchema _ = pure . MkSchema $ Json.object [("type", Json.string "boolean")]

instance ToSchema Text.Text where
  toSchema _ = pure . MkSchema $ Json.object [("type", Json.string "string")]

instance ToSchema Natural.Natural where
  toSchema _ =
    pure . MkSchema $
      Json.object [("type", Json.string "integer"), ("minimum", Json.integer 0)]

instance (ToSchema a) => ToSchema (Maybe a) where
  toSchema _ = toSchema (Proxy.Proxy :: Proxy.Proxy a)
  isOptional _ = True

instance (ToSchema a) => ToSchema [a] where
  toSchema _ = do
    MkSchema items <- toSchema (Proxy.Proxy :: Proxy.Proxy a)
    pure . MkSchema $ Json.object [("type", Json.string "array"), ("items", items)]

instance (ToSchema a) => ToSchema (NonEmpty.NonEmpty a) where
  toSchema _ = do
    MkSchema items <- toSchema (Proxy.Proxy :: Proxy.Proxy a)
    pure . MkSchema $
      Json.object
        [("type", Json.string "array"), ("items", items), ("minItems", Json.integer 1)]

-- * Generic schema encoding

-- | Generic JSON Schema encoding. Dispatches between record encoding
-- (single constructor produces an object schema) and tagged encoding
-- (sum type produces a @oneOf@ schema with tagged variants).
type GToSchema :: (Kind.Type -> Kind.Type) -> Kind.Constraint
class GToSchema f where
  gToSchema :: Proxy.Proxy f -> SchemaM Schema

instance (GToSchema f) => GToSchema (Generics.M1 Generics.D c f) where
  gToSchema _ = gToSchema (Proxy.Proxy :: Proxy.Proxy f)

instance (GToSchemaFields f) => GToSchema (Generics.M1 Generics.C c f) where
  gToSchema _ = do
    fields <- gToSchemaFields (Proxy.Proxy :: Proxy.Proxy f)
    let allProps = fmap (\(n, MkSchema s, _) -> (n, s)) fields
    let reqNames = (\(n, _, _) -> Json.string n) <$> filter (\(_, _, r) -> r) fields
    pure . MkSchema $
      Json.object
        [ ("type", Json.string "object"),
          ("properties", Json.object allProps),
          ("required", Json.array reqNames),
          ("additionalProperties", Json.boolean False)
        ]

instance (GToSchemaSum f, GToSchemaSum g) => GToSchema (f Generics.:+: g) where
  gToSchema _ = do
    variants <- gToSchemaSum (Proxy.Proxy :: Proxy.Proxy (f Generics.:+: g))
    pure . MkSchema $ Json.object [("oneOf", Json.array $ fmap unwrap variants)]

-- | Extract record fields as @(name, schema, required)@ triples.
type GToSchemaFields :: (Kind.Type -> Kind.Type) -> Kind.Constraint
class GToSchemaFields f where
  gToSchemaFields :: Proxy.Proxy f -> SchemaM [(String, Schema, Bool)]

instance
  (TypeLits.KnownSymbol name, ToSchema a) =>
  GToSchemaFields (Generics.M1 Generics.S ('Generics.MetaSel ('Just name) su ss ds) (Generics.K1 i a))
  where
  gToSchemaFields _ = do
    s <- toSchema (Proxy.Proxy :: Proxy.Proxy a)
    let n = TypeLits.symbolVal (Proxy.Proxy :: Proxy.Proxy name)
    let req = not $ isOptional (Proxy.Proxy :: Proxy.Proxy a)
    pure [(n, s, req)]

instance (GToSchemaFields f, GToSchemaFields g) => GToSchemaFields (f Generics.:*: g) where
  gToSchemaFields _ = do
    l <- gToSchemaFields (Proxy.Proxy :: Proxy.Proxy f)
    r <- gToSchemaFields (Proxy.Proxy :: Proxy.Proxy g)
    pure $ l <> r

instance GToSchemaFields Generics.U1 where
  gToSchemaFields _ = pure []

-- | Tagged encoding for sum type constructors.
type GToSchemaSum :: (Kind.Type -> Kind.Type) -> Kind.Constraint
class GToSchemaSum f where
  gToSchemaSum :: Proxy.Proxy f -> SchemaM [Schema]

instance (GToSchemaSum f, GToSchemaSum g) => GToSchemaSum (f Generics.:+: g) where
  gToSchemaSum _ = do
    l <- gToSchemaSum (Proxy.Proxy :: Proxy.Proxy f)
    r <- gToSchemaSum (Proxy.Proxy :: Proxy.Proxy g)
    pure $ l <> r

instance
  (TypeLits.KnownSymbol name) =>
  GToSchemaSum (Generics.M1 Generics.C ('Generics.MetaCons name fix rec) Generics.U1)
  where
  gToSchemaSum _ =
    let n = TypeLits.symbolVal (Proxy.Proxy :: Proxy.Proxy name)
     in pure
          [ MkSchema $
              Json.object
                [ ("type", Json.string "object"),
                  ("properties", Json.object [("type", Json.object [("const", Json.string n)])]),
                  ("required", Json.array [Json.string "type"]),
                  ("additionalProperties", Json.boolean False)
                ]
          ]

instance
  (TypeLits.KnownSymbol name, ToSchema a) =>
  GToSchemaSum (Generics.M1 Generics.C ('Generics.MetaCons name fix rec) (Generics.M1 Generics.S sel (Generics.K1 i a)))
  where
  gToSchemaSum _ = do
    MkSchema valueSchema <- toSchema (Proxy.Proxy :: Proxy.Proxy a)
    let n = TypeLits.symbolVal (Proxy.Proxy :: Proxy.Proxy name)
    pure
      [ MkSchema $
          Json.object
            [ ("type", Json.string "object"),
              ( "properties",
                Json.object
                  [ ("type", Json.object [("const", Json.string n)]),
                    ("value", valueSchema)
                  ]
              ),
              ("required", Json.array [Json.string "type", Json.string "value"]),
              ("additionalProperties", Json.boolean False)
            ]
      ]

instance (GToSchema (Generics.Rep a)) => ToSchema (Generics.Generically a) where
  toSchema _ = gToSchema (Proxy.Proxy :: Proxy.Proxy (Generics.Rep a))

-- * Tests

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = do
  Spec.named s 'toSchema $ do
    Spec.it s "Bool schema has type boolean" $ do
      let (MkSchema v, _) = runSchemaM $ toSchema (Proxy.Proxy :: Proxy.Proxy Bool)
      Spec.assertEq s v $ Json.object [("type", Json.string "boolean")]

    Spec.it s "Natural schema has type integer with minimum 0" $ do
      let (MkSchema v, _) = runSchemaM $ toSchema (Proxy.Proxy :: Proxy.Proxy Natural.Natural)
      Spec.assertEq s v $
        Json.object [("type", Json.string "integer"), ("minimum", Json.integer 0)]

    Spec.it s "Text schema has type string" $ do
      let (MkSchema v, _) = runSchemaM $ toSchema (Proxy.Proxy :: Proxy.Proxy Text.Text)
      Spec.assertEq s v $ Json.object [("type", Json.string "string")]

    Spec.it s "[Bool] schema is array of boolean" $ do
      let (MkSchema v, _) = runSchemaM $ toSchema (Proxy.Proxy :: Proxy.Proxy [Bool])
      Spec.assertEq s v $
        Json.object
          [ ("type", Json.string "array"),
            ("items", Json.object [("type", Json.string "boolean")])
          ]

    Spec.it s "NonEmpty Natural schema has minItems 1" $ do
      let (MkSchema v, _) = runSchemaM $ toSchema (Proxy.Proxy :: Proxy.Proxy (NonEmpty.NonEmpty Natural.Natural))
      Spec.assertEq s v $
        Json.object
          [ ("type", Json.string "array"),
            ("items", Json.object [("type", Json.string "integer"), ("minimum", Json.integer 0)]),
            ("minItems", Json.integer 1)
          ]

    Spec.it s "Maybe is optional" $ do
      Spec.assertEq s (isOptional (Proxy.Proxy :: Proxy.Proxy (Maybe Bool))) True

    Spec.it s "Bool is not optional" $ do
      Spec.assertEq s (isOptional (Proxy.Proxy :: Proxy.Proxy Bool)) False

    Spec.it s "Maybe strips to inner schema" $ do
      let (MkSchema v, _) = runSchemaM $ toSchema (Proxy.Proxy :: Proxy.Proxy (Maybe Bool))
      Spec.assertEq s v $ Json.object [("type", Json.string "boolean")]

    Spec.it s "define returns a $ref" $ do
      let (MkSchema v, _) =
            runSchemaM . define "myBool" $
              toSchema (Proxy.Proxy :: Proxy.Proxy Bool)
      Spec.assertEq s v $ Json.object [("$ref", Json.string "#/$defs/myBool")]

    Spec.it s "define accumulates the definition" $ do
      let (_, defs) =
            runSchemaM . define "myBool" $
              toSchema (Proxy.Proxy :: Proxy.Proxy Bool)
      Spec.assertEq s defs [("myBool", Json.object [("type", Json.string "boolean")])]
