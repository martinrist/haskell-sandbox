# Chapter 6 - Typeclasses

## 6.2 - What are typeclasses?

- Type declarations define how that type in particular is created.  _Typeclasses_ define how sets of types are consumed or used in computations.

- Typeclasses are similar to _interfaces_ in other languages.  They allow us to generalise over a set of types in order to define and execute a standard set of features for those types.

- For example, we want to be able to test values of a range of types for equality:
    - We do not need separate equality functions for each datatype.
    - Instead any datatype that is an instance of the `Eq` typeclass has a `==` operation.
    - If a type is an _instance_ of a typeclass, the type has an implementation for all of the operations defined in the typeclass.


## 6.3 - `Bool` and some common Typeclasses

- Can see the typeclasses that a type instantiates using `:i` in GHCI:

        > :i Bool
        data Bool = False | True        -- Defined in ‘GHC.Types'
        instance Bounded Bool -- Defined in ‘GHC.Enum’
        instance Enum Bool -- Defined in ‘GHC.Enum’
        instance Eq Bool -- Defined in ‘GHC.Classes’
        instance Ord Bool -- Defined in ‘GHC.Classes’
        instance Read Bool -- Defined in ‘GHC.Read’
        instance Show Bool -- Defined in ‘GHC.Show’

- Shows some common typeclasses:
    - `Bounded` - for types that have an upper and lower bound.
    - `Enum` - for types whose values can be enumerated.
    - `Eq` - for types whose values can be tested for equality.
    - `Ord` - for types whose values can be ordered.
    - `Read` - parses strings into instances of the type.
    - `Show` - renders instances of the type into strings.

- Typeclasses can have a hierarchy, e.g. all `Fractional` numbers implement `Num`

        > :i Fractional 
        class Num a => Fractional a where ...


## 6.4 - The `Eq` Typeclass

- `Eq` is the typeclass that allows types to define a notion of equality:

        > :info Eq
        class Eq a where
        (==) :: a -> a -> Bool
        (/=) :: a -> a -> Bool
        ...

- The types of these two functions show the constrained type variable:

        > :t (==)
        (==) :: Eq a => a -> a -> Bool
        > :t (/=)
        (/=) :: Eq a => a -> a -> Bool

- To avoid manually writing instances of common typeclasses, they can be _derived_:

        > data Foo = Bar | Blort deriving Eq
        > Bar == Foo
        False
