# Chapter 16 - Functor

## 16.2 - `Functor` definition

- A _functor_ is a way of applying a function over or around some structure, when we don't want to alter the structure.

- Implemented with the `Functor` typeclass, which defines the `fmap` function:

    ```haskell
    class Functor f where
        fmap :: (a -> b) -> f a -> f b
    ```


## 16.3 - Examples of `fmap`

- `fmap` for lists is the same as `map`:

    ```haskell
    > map (\x -> x + 3) [1..6]
    [4, 5, 6, 7, 8, 9]

    > fmap (\x -> x + 3) [1..6]
    [4, 5, 6, 7, 8, 9]
    ```

- However, `fmap` works over other functors, unlike `map`:

    ```haskell
    > map (+1) (Just 1)
    ... type error

    > fmap (+1) (Just 1)
    Just 2

    > fmap (+1) (2, 3)
    (2, 4)
    ```


## 16.4 - The _f_ in `Functor f`

- The _f_ in the typeclass definition for `Functor` must be the same across the definition, and it must refer to to a type that implements the typeclass.

- _f_ must have the kind `* -> *`:
    - i.e. it is a _higher-kinded type_, that is awaiting application to a type constant of kind `*`.
    - We know this because every argument to the type constructor of `->` must be of kind `*` (`:k (->)` is `* -> * -> *`).
    - Therefore, in the signature of `fmap` both the `f a` and `f b` must be of kind `*`.
    - Therefore, the `f` must be of kind `* -> *`.

- There is also an infix operator `<$>` for `fmap`:

    ```haskell
    > :t (<$>)
    <$> :: Functor f => (a -> b) -> f a -> f b
    ```

- Note the similarity between `<$>` and `$` - this is because `fmap / <$>` is like function application (`$`) 'lifted' over the functor.

- Here's an example of implementing `Functor`:

    ```haskell
    data FixMePls a =
        FixMe
      | Pls a
      deriving (Eq, show)

    instance Functor FixMePls where
        fmap _ FixMe   = FixMe
        fmap f (Pls a) = Pls (f a)
    ```


## 16.5 - `Functor` laws

- Like `Monoid`, instances of the `Functor` typeclass should satisfy laws:

    ```haskell
    -- Identity - mapping `id` over a Functor should leave it unchanged
    fmap id == id

    -- Composability - mapping the composition is the same as mapping each function in turn
    fmap (f . g) == fmap f . fmap g
    ```


## 16.6 - Examples of broken `Functor`s

- A simple functor that breaks the identity law:

    ```haskell
    data WhoCares a =
        ItDoesnt
      | Matter a
      | WhatThisIsCalled
    deriving (Eq, Show)

    instance Functor WhoCares where
        fmap _ ItDoesnt         = WhatThisIsCalled
        fmap _ WhatThisIsCalled = ItDoesnt
        fmap f (Matter a)       = Matter (f a)

    > fmap id ItDoesnt == id ItDoesnt
    False
    ```

- Example of breaking composition, because we mess with part of the structure (by incrementing the `n`:

    ```haskell
    data CountingBad a =
        Heisenberg Int a
        deriving (Eq, Show)

    instance Functor CountingBad where
        fmap f (Heisenberg n a) = Heisenberg (n+1) (f a)

    > let f = (++ " Jesse")
    > let g = (++ " lol")
    > let u = Heisenberg 0 "Uncle"

    > fmap (f . g) u
    Heisenberg 1 "Uncle lol Jesse"

    > fmap f . fmap g $ u
    Heisenberg 2 "Uncle lol Jesse" 
    ```
