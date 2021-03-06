# Chapter 1 - The Algebra Behind Types - Exercises

## Exercise 1.2-i

Determine the cardinality of `Either Bool (Bool, Maybe Bool) -> Bool`:

```haskell
  |Either Bool (Bool, Maybe Bool) -> Bool|
= |Bool| ^ |Either Bool (Bool, Maybe Bool)|
= |Bool| ^ (|Bool| + |(Bool, Maybe Bool)|)
= |Bool| ^ (|Bool| + (|Bool| * |Maybe Bool|))
= |Bool| ^ (|Bool| + (|Bool| * ( 1 + |Bool|)))
= 2 ^ (2 + (2 * (1 + 2)))
= 2 ^ 8
= 256
```


## Exercise 1.4-i

_Use Curry–Howard to prove that `(a^b)^c = a^(b×c)`.  That is, provide a
function of type `(b -> c -> a) -> (b, c) -> a`, and one of
`((b,c) -> a) -> b -> c - >a`.  Make sure they satisfy the equalities
`to . from = id` and `from . to = id`.  Do these functions remind you of
anything from Prelude?_

See [Exercises.hs](Exercises.hs) for implementations and [ExercisesSpec.hs](../../../test/ThinkingWithTypes/Chapter01/ExercisesSpec.hs) for tests.

These functions are basically `Prelude`'s [`curry`](https://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html#v:curry) and [`uncurry`](https://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html#v:uncurry).


## Exercsie 1.4-ii

_Give a proof of the exponent law that `a^b x a^c = a ^ (b + c)`_

This is equivalent to implementing the following functions:

```haskell
from :: (b -> a, c -> a) -> Either b c -> a
to   :: (Either b c -> a) -> (b -> a, c -> a)
```

See [Exercises.hs](Exercises.hs) for implementations and [ExercisesSpec.hs](../../../test/ThinkingWithTypes/Chapter01/ExercisesSpec.hs) for tests.


## Exercise 1.4-iii

_Prove `(a x b) ^ c = a^c x b^c`_

This is equivalent to implementing the following functions:

```haskell
from :: (c -> (a, b)) -> (c -> a, c -> b)
to   :: (c -> a, c -> b) -> c -> (a, b)
```

See [Exercises.hs](Exercises.hs) for implementations and [ExercisesSpec.hs](../../../test/ThinkingWithTypes/Chapter01/ExercisesSpec.hs) for tests.