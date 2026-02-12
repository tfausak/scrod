{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Scrod.Core.ItemKind where

import qualified GHC.Generics as Generics
import qualified Scrod.Json.ToJson as ToJson
import qualified Scrod.Schema as Schema

-- | The kind of Haskell declaration an Item represents.
data ItemKind
  = -- | Function binding: @f x = expr@
    Function
  | -- | Pattern binding: @(x, y) = tuple@
    PatternBinding
  | -- | Pattern synonym: @pattern P x = Just x@
    PatternSynonym
  | -- | Data type declaration: @data T = C@
    DataType
  | -- | Newtype declaration: @newtype T = C@
    Newtype
  | -- | Type data declaration: @type data T = C@
    TypeData
  | -- | Type synonym: @type T = U@
    TypeSynonym
  | -- | Data constructor (Haskell 98 style)
    DataConstructor
  | -- | GADT constructor
    GADTConstructor
  | -- | Record field
    RecordField
  | -- | Type class: @class C a@
    Class
  | -- | Class method signature
    ClassMethod
  | -- | Class instance: @instance C T@
    ClassInstance
  | -- | Standalone deriving: @deriving instance C T@
    StandaloneDeriving
  | -- | Derived instance from deriving clause
    DerivedInstance
  | -- | Open type family (standalone or associated)
    OpenTypeFamily
  | -- | Closed type family
    ClosedTypeFamily
  | -- | Data family (standalone or associated)
    DataFamily
  | -- | Type family instance: @type instance F T = U@
    TypeFamilyInstance
  | -- | Data family instance: @data instance D T = C@
    DataFamilyInstance
  | -- | Foreign import declaration
    ForeignImport
  | -- | Foreign export declaration
    ForeignExport
  | -- | Fixity signature: @infixl 6 +@
    FixitySignature
  | -- | Inline/noinline signature
    InlineSignature
  | -- | Specialise signature
    SpecialiseSignature
  | -- | Standalone kind signature: @type T :: Type@
    StandaloneKindSig
  | -- | Rewrite rule: @{-# RULES ... #-}@
    Rule
  | -- | Default declaration: @default (Integer, Double)@
    Default
  | -- | Annotation: @{-# ANN ... #-}@
    Annotation
  | -- | Template Haskell splice or quasi-quote: @$(expr)@ or @[quoter|...|]@
    Splice
  deriving (Eq, Generics.Generic, Ord, Show)
  deriving (ToJson.ToJson, Schema.ToSchema) via Generics.Generically ItemKind
