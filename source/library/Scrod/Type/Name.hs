module Scrod.Type.Name where

data Name
  = -- | @class X@
    Class String
  | -- @instance X@
    ClassInstance String
  | -- | @type family X where@
    ClosedTypeFamily String
  | -- | @data A = X@
    Constructor String
  | -- @data X@
    Data String
  | -- | @data family X@
    DataFamily String
  | -- | @data instance X@
    DataInstance String
  | -- | @data A = B { x :: C }@
    Field String
  | -- | @data A where X :: A@
    --
    -- Note that @X@ may not actually be a GADT. It's just defined with GADT
    -- syntax.
    GADT String
  | -- | @newtype X = A ()@
    Newtype String
  | -- | @newtype instance X = A ()@
    NewtypeInstance String
  | -- | @type family X@
    OpenTypeFamily String
  | -- | @type data X@
    TypeData String
  | -- | @type instance X = A@
    TypeInstance String
  | -- | @type X@
    TypeSynonym String
  | Other String
  deriving (Eq, Show)
