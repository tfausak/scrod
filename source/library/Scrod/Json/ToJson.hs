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

instance ToJson Natural.Natural where
  toJson = Json.integral

instance (ToJson a) => ToJson (Maybe a) where
  toJson = maybe Json.null toJson

instance (ToJson a) => ToJson [a] where
  toJson = Json.arrayOf toJson

instance (ToJson a) => ToJson (NonEmpty.NonEmpty a) where
  toJson = toJson . NonEmpty.toList

-- | Generic derivation helper for record types. Produces a list of
-- key-value pairs from the generic representation.
class GToJson f where
  gToJsonFields :: f p -> [(String, Json.Value)]

instance (GToJson f) => GToJson (Generics.M1 Generics.D c f) where
  gToJsonFields (Generics.M1 x) = gToJsonFields x

instance (GToJson f) => GToJson (Generics.M1 Generics.C c f) where
  gToJsonFields (Generics.M1 x) = gToJsonFields x

instance
  (TypeLits.KnownSymbol name, ToJson a) =>
  GToJson (Generics.M1 Generics.S ('Generics.MetaSel ('Just name) su ss ds) (Generics.K1 i a))
  where
  gToJsonFields (Generics.M1 (Generics.K1 x)) =
    [(TypeLits.symbolVal (Proxy.Proxy :: Proxy.Proxy name), toJson x)]

instance (GToJson f, GToJson g) => GToJson (f Generics.:*: g) where
  gToJsonFields (f Generics.:*: g) = gToJsonFields f <> gToJsonFields g

instance GToJson Generics.U1 where
  gToJsonFields Generics.U1 = []

instance (Generics.Generic a, GToJson (Generics.Rep a)) => ToJson (Generics.Generically a) where
  toJson (Generics.Generically x) = Json.object (gToJsonFields (Generics.from x))

-- | Wrapper for deriving 'ToJson' for enum types (nullary constructors
-- only). The constructor name becomes a JSON string.
newtype GenericEnum a = GenericEnum a

-- | Generic derivation helper for enum types. Extracts the constructor
-- name as a 'String'.
class GToJsonEnum f where
  gToJsonEnum :: f p -> String

instance (GToJsonEnum f) => GToJsonEnum (Generics.M1 Generics.D c f) where
  gToJsonEnum (Generics.M1 x) = gToJsonEnum x

instance (GToJsonEnum f, GToJsonEnum g) => GToJsonEnum (f Generics.:+: g) where
  gToJsonEnum (Generics.L1 x) = gToJsonEnum x
  gToJsonEnum (Generics.R1 x) = gToJsonEnum x

instance
  (TypeLits.KnownSymbol name) =>
  GToJsonEnum (Generics.M1 Generics.C ('Generics.MetaCons name fix rec) Generics.U1)
  where
  gToJsonEnum _ = TypeLits.symbolVal (Proxy.Proxy :: Proxy.Proxy name)

instance (Generics.Generic a, GToJsonEnum (Generics.Rep a)) => ToJson (GenericEnum a) where
  toJson (GenericEnum x) = Json.string (gToJsonEnum (Generics.from x))
