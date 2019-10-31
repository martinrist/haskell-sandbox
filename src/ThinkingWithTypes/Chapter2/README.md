# Chapter 2 - Terms, Types & Kinds

## Page Contents
- [Chapter 2 - Terms, Types & Kinds](#chapter-2---terms-types--kinds)
  - [Page Contents](#page-contents)
  - [See Also](#see-also)
  - [The Kind System](#the-kind-system)
  - [Data Kinds](#data-kinds)
  - [Promotion of Built-in Types](#promotion-of-built-in-types)
  - [Type-Level Functions](#type-level-functions)


## See Also

- [Exercises - Text](Exercises.md)
- [Examples - Source Code](Examples.hs)
- [Exercsies - Source Code](Exercises.hs)


## The Kind System

- In 'vanilla' Haskell, we typically work with _terms_ and _types_:
  - _Terms_ - things that we manipulate, and exist at runtime.
  - _Types_ - compile-time 'sanity checks' - proofs to the compiler that what
    we're writing makes sense.

- Analogously, the building blocks for type-level programming are _types_ and
_kinds_:
    - _Types_ - these become the things we manipulate.
    - _Kinds_ - these become the 'proofs' to the compiler, they are like h

- **Note:** Whilst the word 'type' can be used to refer to things that exist at
the type level, we can also refer to `Type`, which is the _kind_  of
inhabited types (i.e. types that have at least one value).   `Type` is
typically shown as `*` in current versions of GHCi:

    ```haskell
    > :k Int
    Int :: *

    > :k Maybe Int
    Maybe Int :: *
    ```

- _Higher-kinded types (HKTs)_ are those which have type variables.
Fully-saturdated HKTs have kind `*` / `TYPE`, but their data constructors do
not, e.g.:

    ```haskell
    > :k Maybe Int
    Maybe Int :: *          -- As above, since `Maybe Int` is fully-saturated

    > :k Maybe
    Maybe :: * -> *         -- `Maybe` takes something of kind `*`
                            -- and returns something of kind `*`

    > :k Either
    Either :: * -> * -> *   -- `Either` takes two type parameters

    > :k Either Int
    Either Int :: * -> *    -- 'Partially applying one type paramter
    ```

- There are more exciting HKTs (e.g. the Monad Transformer [`MaybeT`](http://hackage.haskell.org/package/transformers-0.5.6.2/docs/Control-Monad-Trans-Maybe.html#t:MaybeT)).  This takes a monad like `Maybe`
(which has kind `* -> *` and a type, and returns a type, so:

    ```haskell
    > import Control.Monad.Trans.Maybe
    > :k MaybeT
    MaybeT :: (* -> *) -> * -> *
    ```

- Kinds also apply to other things at the type level, as well as tradiional
types.  For example, the type signature of `show` is:

    ```haskell
    > :t show
    show :: Show a => a -> String
    ```

    `Show a` is part of the type signature, but it's not a type.  It's a
    _constraint_ and hence has kind `Constraint`:

    ```haskell
    > :k Show Int
    Show Int :: Constraint     -- A fully-saturated constraint

    > :k Show
    Show :: * -> Constraint    -- Takes a type and returns a constraint
    ```

- So far, this is all we get with 'vanilla' Haskell 2010.

## Data Kinds

- Enabling the [`-XDataKinds`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#datatype-promotion) extensions allows us to start talking about kinds other than `*`, `Constraint` and their arrow-derivatives.

- `-XDataKinds` automatically lifts data constructors into _type constructors_
and types into _kinds_:

    ```haskell
    > :set -XDataKinds
    > data Answer = Yes | No
    ```

- In vanilla Haskell 2010, the above definiton introduces the following into
scope:
  - A type constructor `Answer` of kind `*`
  - Data constructors `Yes` and `No` of type `Answer`

    ```haskell
    > :k Answer
    Answer :: *         -- Type constructor
    > :t Yes
    Yes :: Answer       -- Data constructor
    > :t No
    No :: Answer        -- Data constructor
    ```

- With `-XDataKinds` enabled we also get:
  - A new kind, `Answer`
  - Promoted data constructors `'Yes'` and `'No` of kind `Answer`:

    ```haskell
    > :k 'Yes
    'Yes :: Answer
    > :k 'No
    'No :: Answer
    ```

- Note the ticks in the promoted data constructors `'Yes` and `'No`', which are
used to distinguish promoted data constructors from regular type constructors
(since they all exist in the same namespace).


## Promotion of Built-in Types


## Type-Level Functions