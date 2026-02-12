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
