# Chapter 6 - Rank-N Types

## Page Contents
- [Chapter 6 - Rank-N Types](#chapter-6---rank-n-types)
  - [Page Contents](#page-contents)
  - [See Also](#see-also)
  - [Introduction](#introduction)


## See Also

- [Examples - Source Code](Examples.hs)
- [Exercises - Source Code](Exercises.hs)
- [Exercises - Tests](../../../test/ThinkingWithTypes/Chapter06/ExercisesSpec.hs)


## Introduction

- Consider a first attempt at writing a function that takes `id :: a -> a` as
an argument and applies it to the number `5`:

    ```haskell
    applyToFiveBroken :: (a -> a) -> Int
    applyToFiveBroken f = f 5
  ```

  This gives us the following error message, because in the definition of
  `applyToFive`, we're trying to make `f` take an `Int` (`5`), whereas all
  we know from the signature is that it's a `a -> a`:

    ```haskell
    Couldn't match expected type ‘Int’ with actual type ‘a’
    ```

- Under normal circumstances, it's the _caller_ of a polymorphic function that
is responsible for choosing which concrete types those variables get.  Since
the function `applyToFive` is `(a -> a) -> Int`, we're saying that it promises
to take _any_ function `a -> a` (i.e. any _endomorphism_).

- The type signature `a -> a` is really syntactic sugar for `forall a. a -> a` -
the type variable `a` is automatically quantified.

- By enabling the [`RankNTypes`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-RankNTypes) extension we can write
these types explicitly.

- With `RankNTypes`, the original type signature is equivalent to:

    ```haskell
    applyToFiveBroken :: forall a . (a -> a) -> Int
    ```

  i.e.

    ```haskell
    applyToFiveBroken :: forall a . ( (a -> a) -> Int )
    ```

- To make it work, we just need to move the `forall a` so that it only applies
to the first parameter

    ```haskell
    applyToFive :: (forall a. a -> a) -> Int
    applyToFive f = f 5

    > Then, in GHCi
    > :t applyToFive
    applyToFive :: (forall a. a -> a) -> Int

    > applyToFive id
    5
    ```