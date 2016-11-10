# Chapter 6 - Typeclasses

## 6.2 - What are typeclasses?

- Type declarations define how that type in particular is created.  _Typeclasses_ define how sets of types are consumed or used in computations.

- Typeclasses are similar to _interfaces_ in other languages.  They allow us to generalise over a set of types in order to define and execute a standard set of features for those types.

- For example, we want to be able to test values of a range of types for equality:
    - We do not need separate equality functions for each datatype.
    - Instead any datatype that is an instance of the `Eq` typeclass has a `==` operation.
    - If a type is an _instance_ of a typeclass, the type has an implementation for all of the operations defined in the typeclass.


## 6.3 - `Bool` and some common typeclasses

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


## 6.4 - The `Eq` typeclass

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


## 6.5 - Writing typeclass instances

- To avoid manually writing instances of common typeclasses, they can be _derived_:

        > data Foo = Bar | Blort deriving Eq
        > Bar == Foo
        False

- To define an `Eq` instance, you just need to implement _either_ `(==)` or `(/=)`:

        data Trivial = Trivial
        instance Eq Trivial where
            Trivial == Trivial = True

- Recommended to keep the typeclass instances for a type in the same file as that type.

- When writing cases for a function, be careful to make sure we don't end up with a _partial function_ - one that doesn't handle all cases
    - Set GHCi's `Wall` flag to show warning for partial functions: `:set -Wall`.

- Sometimes we need the type variable to have certain typeclass instances:

        data Identity a = Identity a
        instance Eq (Identity a) where
            (==) (Identity v) (Identity v') = v == v'

  This won't work - we can't guarantee to be able to call `v == v'` since the type variable `a` is not constrained to have an instance of `Eq`.  Instead we use a typeclass constraint:

        instance Eq a => Eq (Identity a) where
            (==) (Identity v) (Identity v') = v == v'
