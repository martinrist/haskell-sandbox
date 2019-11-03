# Chapter 3 - Variance

## Exercise 3-i

_Which of the following types are `Functor`s?  Give instances for the ones that are:_

  ```haskell
  newtype T1 a = T1 (Int -> a)

  newtype T2 a = T2 (a -> Int)

  newtype T3 a = T3 (a -> a)

  newtype T4 a = T4 ((Int -> a) -> Int)

  newtype T5 a = T5 ((a -> Int) -> Int)
  ```

Only `T1` and `T5` have `Functor` instances.  See [Exercises.hs](Exercises.hs)
for implementations, and see [ExercisesSpec.hs](../../../test/ThinkingWithTypes/Chapter03/ExercisesSpec.hs)
for QuickCheck tests confirming that they are valid `Functor` instances.