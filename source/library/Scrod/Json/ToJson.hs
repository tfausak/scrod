{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Type class for converting values into JSON.
module Scrod.Json.ToJson where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Proxy as Proxy
import qualified Data.Text as Text
import qualified GHC.Generics as Generics
import qualified GHC.TypeLits as TypeLits
import qualified Numeric.Natural as Natural
import qualified Scrod.Json.Value as Json

-- | Convert a value to a JSON 'Json.Value'.
class ToJson a where
  toJson :: a -> Json.Value

instance ToJson Bool where
  toJson = Json.boolean

instance ToJson Text.Text where
  toJson = Json.text

instance ToJson Int where
  toJson = Json.integral

instance ToJson Natural.Natural where
  toJson = Json.integral

instance (ToJson a) => ToJson (Maybe a) where
  toJson = maybe Json.null toJson

instance (ToJson a) => ToJson [a] where
  toJson = Json.arrayOf toJson

instance (ToJson a) => ToJson (NonEmpty.NonEmpty a) where
  toJson = toJson . NonEmpty.toList

-- | Generic JSON encoding. Dispatches between record encoding (single
-- constructor produces a JSON object) and tagged encoding (sum type
-- produces @{\"type\": \"Name\", \"value\": ...}@ objects).
--
-- Use @deriving via 'Generics.Generically'@ with a 'Generics.Generic'
-- instance to derive 'ToJson' for record types, enum types, and tagged
-- sum types.
class GToJson f where
  gToJson :: f p -> Json.Value

instance (GToJson f) => GToJson (Generics.M1 Generics.D c f) where
  gToJson (Generics.M1 x) = gToJson x

instance (GToJsonFields f) => GToJson (Generics.M1 Generics.C c f) where
  gToJson (Generics.M1 x) =
    Json.object (filter (\(_, v) -> v /= Json.null) (gToJsonFields x))

instance (GToJsonSum f, GToJsonSum g) => GToJson (f Generics.:+: g) where
  gToJson = gToJsonSum

-- | Extract record fields as key-value pairs for JSON object encoding.
class GToJsonFields f where
  gToJsonFields :: f p -> [(String, Json.Value)]

instance
  (TypeLits.KnownSymbol name, ToJson a) =>
  GToJsonFields (Generics.M1 Generics.S ('Generics.MetaSel ('Just name) su ss ds) (Generics.K1 i a))
  where
  gToJsonFields (Generics.M1 (Generics.K1 x)) =
    [(TypeLits.symbolVal (Proxy.Proxy :: Proxy.Proxy name), toJson x)]

instance (GToJsonFields f, GToJsonFields g) => GToJsonFields (f Generics.:*: g) where
  gToJsonFields (f Generics.:*: g) = gToJsonFields f <> gToJsonFields g

instance GToJsonFields Generics.U1 where
  gToJsonFields Generics.U1 = []

-- | Tagged encoding for sum type constructors.
class GToJsonSum f where
  gToJsonSum :: f p -> Json.Value

instance (GToJsonSum f, GToJsonSum g) => GToJsonSum (f Generics.:+: g) where
  gToJsonSum (Generics.L1 x) = gToJsonSum x
  gToJsonSum (Generics.R1 x) = gToJsonSum x

instance
  (TypeLits.KnownSymbol name) =>
  GToJsonSum (Generics.M1 Generics.C ('Generics.MetaCons name fix rec) Generics.U1)
  where
  gToJsonSum _ =
    Json.object [("type", Json.string (TypeLits.symbolVal (Proxy.Proxy :: Proxy.Proxy name)))]

instance
  (TypeLits.KnownSymbol name, ToJson a) =>
  GToJsonSum (Generics.M1 Generics.C ('Generics.MetaCons name fix rec) (Generics.M1 Generics.S sel (Generics.K1 i a)))
  where
  gToJsonSum (Generics.M1 (Generics.M1 (Generics.K1 x))) =
    Json.tagged (TypeLits.symbolVal (Proxy.Proxy :: Proxy.Proxy name)) (toJson x)

instance (Generics.Generic a, GToJson (Generics.Rep a)) => ToJson (Generics.Generically a) where
  toJson (Generics.Generically x) = gToJson (Generics.from x)
