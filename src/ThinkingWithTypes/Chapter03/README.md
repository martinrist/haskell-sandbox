# Chapter 3 - Variance

## Page Contents
- [Chapter 3 - Variance](#chapter-3---variance)
  - [Page Contents](#page-contents)
  - [See Also](#see-also)
  - [Functor Instances](#functor-instances)
  - [Types of Variance](#types-of-variance)
  - [Determining Variance](#determining-variance)
  - [Bifunctors and Profunctors](#bifunctors-and-profunctors)


## See Also

- [Examples - Source Code](Examples.hs)
- [Exercises - Text](Exercises.md)
- [Exercises - Source Code](Exercises.hs)
- [Exercises - Tests](../../../test/ThinkingWithTypes/Chapter03/ExercisesSpec.hs)


## Functor Instances

- Of the following type declarations, only `T1` and `T5` are `Functor`s:

    ```haskell
    newtype T1 a = T1 (Int -> a)

    newtype T2 a = T2 (a -> Int)

    newtype T3 a = T3 (a -> a)

    newtype T4 a = T4 ((Int -> a) -> Int)

    newtype T5 a = T5 ((a -> Int) -> Int)
    ```

- The reason for this is _variance_, which describes whether, if we can
transform an `a` into a `b`, can we transform a `T a` into a `T b`.


## Types of Variance

- There are three possibilities for `T`'s variance with respect to its type
constructor `a`:
  - _Covariant_ - any function `a -> b` can be lifted into a function
    `T a -> T b`.
  - _Contravariant_ - any function `a -> b` can be lifted into a function
    `T b -> T a`.
  - _Invariant_ - in general, functions `a -> b` cannot be lifted into a
    function over `T`.

- Covariance corresponds to having an instance of `Functor` - i.e. having
a function `fmap :: (a -> b) -> T a -> T b`.

- The [`Contravariant`](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Functor-Contravariant.html#t:Contravariant)
and [`Invariant`](https://hackage.haskell.org/package/invariant-0.5.3/docs/Data-Functor-Invariant.html#t:Invariant)
typeclasses define classes for the remaining types of variance.

- [`Contravariant`](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Functor-Contravariant.html#t:Contravariant)
allows us to map a function _backwards_ across its type constructor:

    ```haskell
    class Contravariant f where
        contramap :: (a -> b) -> f b -> f a
    ```

- [`Invariant`](https://hackage.haskell.org/package/invariant-0.5.3/docs/Data-Functor-Invariant.html#t:Invariant)
allows us to map from `a` to `b` iff they are isomorphic:

    ```haskell
    class Invariant f where
        invmap :: (a -> b) -> (b -> a) -> f a -> f b
    ```


## Determining Variance

- The variance of `T a` with respect to `a` is specified by whether `a` appears
in _positive_ or _negative_ position:
    - `a` solely in _positive_ position => covariant
    - `a` solely in _negative_ position => contravariant
    - `a` in a mixture of _positive_ and _negative_ position => invariant

- Positions of type variables can be determined from the following primitives:

    |Type        |Position of `a`|Position of `b`|
    |------------|---------------|---------------|
    |`Either a b`|Positive       |Positive       |
    |`(a, b)`    |Positive       |Positive       |
    |`a -> b`    |Negative       |Positive       |

  i.e. only negative if on the left hand side of a function arrow.

- Variances compose just like multiplication.  Consider, for example
`(a, Bool) -> Int`.  The `a` in the tuple is in +ve position, but the
tuple overall is in -ve position, so overall `a` is in -ve position,
and `(a, Bool) -> Int` is contravariant with respect to `a`.

- Type variance alsohas a more concrete interpretation:
  - Variables in _positive_ position are _produced_ or _owned_ - products,
    sums and the right-side of arrows all exist already or are produced.
  - Variables in _negative_ position are _consumed_ - arguments to functions
    are consumed.


## Bifunctors and Profunctors

- If types have multiple type variables, they can have special names:
    - Covariant in two arguments (e.g. `Either`) => _bifunctors_.
    - Contravariant in first argument, covariant in second (e.g. `->`) =>
        _profunctors_.