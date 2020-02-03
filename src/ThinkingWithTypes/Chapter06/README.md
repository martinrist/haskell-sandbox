# Chapter 6 - Rank-N Types

## Page Contents
- [Chapter 6 - Rank-N Types](#chapter-6---rank-n-types)
  - [Page Contents](#page-contents)
  - [See Also](#see-also)
  - [Introduction](#introduction)
  - [Ranks](#ranks)
  - [Working out the Rank of a Function](#working-out-the-rank-of-a-function)
  - [The Continuation Monad](#the-continuation-monad)


## See Also

- [Examples - Source Code](Examples.hs)
- [Exercises - Text](Exercises.md)
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


## Ranks

- The `RankNTypes` extension allows us to introduce polymorphism anywhere a
type is allowed, rather than just on top-level bindings.  However, there are
some complexities - for example, type inference is undecidable in the presence
of higher-rank polymorphism, so it alwasy requires an explicit type signature.

- The _rank_ of a function is the "depth" of its polymorphism, e.g.:
  - A function `f :: Int -> Int` is _rank 0_ because it has no polymorphic
  parameters.

  - A function `g :: a -> b` is _rank 1_.

  - `applyToFive :: (forall a. a -> a) -> Int` is _rank 2_, because its first
  parameter is itself _rank 1_.

- Intuitively, higher-rank types are _functions which take callbacks_ where the
rank of the function is how often control gets 'handed off', e.g.:
  - A _rank 2_ function will run a callback for you.
  - A _rank 3_ function will run a callback which will itself run a callback.

- Consider an arbitrary _rank 2_ function:

    ```haskell
    foo :: forall r. (forall a. a -> r) -> r
    ```

  Here, as the caller of `foo` we are responsible for determining how `r` is
  instantiated.  However, the _implementation_ of `foo` gets to choose what
  type `a` is.

- Recall:

    ```haskell
    applyToFive :: (forall a. a -> a) -> Int
    applyToFive f = f 5
    ```

  Here, it's the _implementation_ of `applyToFive` that calls `f`.  Since `f`
  is _rank 1_, `applyToFive` can instantiate it at `Int`.

  In the broken version:

    ```haskell
    applyToFiveBroken :: forall a. (a -> a) -> Int
    applyToFive f 5
    ```

  Here, `f` is _rank 0_, because the caller of `applyToFive` has already
  instantiated `a` by the time `applyToFive` gets to access it.  By pushing
  `f` up to _rank 1_ we can delay who gets to decide the type of `a`.


## Working out the Rank of a Function

- A function gains a higher rank every time a `forall` quantifier appears on
the left-hand side of a function arrow.

- The `forall` quantifier binds more loosely than the arrow type `->`, so the
actual type of `id :: forall a. a -> a` is `id :: forall a. (a -> a)`.  In
other words, the arrow is captured by the `forall`.

- Consider a more complex rank-_n_ type with all implicit parentheses inserted:

    ```haskell
    foo :: forall r. ((forall a. (a -> r)) -> r)
    ```

  here, the `forall a` _is_ to the left of the outermost function arrow.  So,
  the rank of a function is the number of arrows its deepest `forall` is to
  the left of.


## The Continuation Monad

- The types `a` and `forall r. (a -> r) -> r` are isomorphic - in other words,
having a value is just like having a function that will give that value to a
callback:

    ```haskell
    cont :: a -> (forall r. (a -> r) -> r)
    cont a = \callback -> callback a

    runCont :: (forall r. (a -> r) -> r) -> a
    runCont f =
      let callback = id
        in f callback
    ```

- THe type `forall r. (a -> r) -> r` is known as being in _continuation-passing
style (CPS)_.

- We know that isomorphism is transitive, and that `Identity a` is isomorphic
to `a`, so there is a transitive isomorphism between `Identity a` and CPS.

- Moreover, `Identity a` is a `Monad` and isomorphisms preserve typeclasses,
so we should expect that CPS forms a `Monad`.  We'll create a `Cont` `newtype`
to attach the various instances to:

    ```haskell
    newtype Cont a = Cont
      { unCont :: forall r. (a -> r) -> r
      }
    ```