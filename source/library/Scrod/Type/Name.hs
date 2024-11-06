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
  | -- | @class C where default m :: X@
    DefaultMethodSignature String
  | -- | @data A = B { x :: () }@
    Field String
  | -- | @infix 5 %@
    Fixity String
  | -- | @foreign import ccall \"a\" x :: ()@
    ForeignImport String
  | -- | @data A where X :: ()@
    --
    -- Note that @X@ may not actually be a GADT. It's just defined with GADT
    -- syntax.
    GADT String
  | -- | @type X :: ()
    KindSignature String
  | -- | @class C where m :: X@
    MethodSignature String
  | -- | @newtype X = A ()@
    Newtype String
  | -- | @newtype instance X = A ()@
    NewtypeInstance String
  | -- | @type family X@
    OpenTypeFamily String
  | -- | @pattern X :: ()@
    PatternSignature String
  | -- | @pattern X = ()@
    PatternSynonym String
  | -- | @type role X nominal@
    Role String
  | -- | @{-# rules \"x\" () = () #-}@
    Rule String
  | -- | @type data X@
    TypeData String
  | -- | @type instance X = ()@
    TypeInstance String
  | -- | @x :: ()@
    TypeSignature String
  | -- | @type X@
    TypeSynonym String
  | -- | @x = ()@
    --
    -- Note that this can either be a variable or a function. In practice this
    -- distinction is not important, since you can write either @fun x = x@ or
    -- @var = \\ x -> x@.
    Variable String
  deriving (Eq, Show)
