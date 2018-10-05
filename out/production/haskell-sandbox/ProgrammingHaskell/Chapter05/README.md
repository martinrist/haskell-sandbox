# Chapter 5 - Types

## 5.3 - How to read type signatures

- Querying the type of a general number says it's a _constrained_ polymorphic number:

    ```haskell
    > :t 13
    13 :: Num a => a
    ```

- We can give the number a _concrete_ type by declaring it explicitly:

    ```haskell
    > let x = 13 :: Integer
    > :t x
    x :: Integer
    ```

- Can also query the type signatures of functions:

    ```haskell
    > :t not
    not :: Bool -> Bool
    ```

- `(->)` is the type constructor for functions:

    ```haskell
    > :i (->)
    data (->) a b  -- Defined in 'GHC.Prim'
    ... and more ...
    ```

- `->` is an infix operator that has two operands and associates to the right:

    ```haskell
    > :t fst
    fst :: (a, b) -> a
    ```

    So `fst` is a function from a tuple `(a, b)` to an `a`.

- Compiler gives the least specific and most general type it can, often resulting in a _typeclass-constrained polymorphic type variable_:

    ```haskell
    > :t (+)
    (+) :: Num a => a -> a -> a
    ```

    Here, we don't know the exact concrete type of `a` but we know it can only be a type that has the `Num` typeclass instance.

- A type signature might have multiple typeclass constraints on one or more of the variables:

    ```haskell
    (Num a, Num b) => a -> b -> b
    (Ord a, Num a) => a -> a -> Ordering
    ```


# 5.4 - Currying

- All Haskell functions take one argument and return one result.

- Syntactic conventions conventions are used to construct _curried_ functions.

- Each `->` in a type signature represents one argument and one result, with the final type being the final result.

- Look at the type signature for addition:

    ```haskell
    (+) :: Num a => a -> a -> a
    ```

  Since `->` is right-associative, this is equivalent to:

    ```haskell
    (+) :: Num a => a -> (a -> a)
    ```

  i.e. `(+)` is a function which takes an `a` and returns (a function which takes an `a` and returns an `a`).

- _Partial application_ allows us to take a function and apply some of its arguments to get another function, e.g.:

    ```haskell
    > let add10 = (+ 10)
    > add10 5
    15
    ```

- It is possible to _uncurry_ functions, replacing the curried version with an uncurried version that takes a tuple parameter containing all the arguments.

- _Sectioning_ refers to partial application of infix operators, with a special syntax:

    ```haskell
    > let x = 5
    > let y = (2 ^)
    > let z = (^ 2)
    > y x
    32
    > z x
    25
    ```

- Sectioning also works with other non-arithmetic operators, but may need to use backticks to make prefix functions infix:

    ```haskell
    > elem 9 [1..10]
    True
    > 9 `elem` [1..10]
    True
    > (`elem` [1..10]) 9
    True
    ```

## 5.5 - Polymorphism

- _Polymorphic_ type variables allow us to implement expressions that can accept arguments and return results of different types without having to write various implementations for each type.

- Two categories of polymorhism in Haskell:
    - _Parametric polymorphism_ - type variables are fully polymorphic - when unconstrained by a typeclass, their final concrete type can be anything.
    - _Constrained polymorphism_, a.k.a. _ad-hoc polymorphism_ - puts typeclass constraints onto type variables, which decreases the number of concrete types that the variable could be, but increases what you can do with it.

- `id :: a -> a` is maximally polymorphic, and can operate on any type.  However, it can only do one thing - i.e. return what it was passed.

- `negate :: Num a => a -> a` constrains `a` to be an instance of the `Num` typeclass, which means it can apply to fewer concrete types, but more can be done with it.

- Concrete types have even more flexibility in terms of computation, because typeclasses are _additive_.  For example, `Int` is an instance of many typeclasses (e.g. `Eq`, `Num`, `Ord` etc), so it can use methods from any of these.

- A function is polymorphic when its type signature has type variables that are polymorphic.

- Numeric literals are polymorphic and stay so until given a more concrete type:

    ```haskell
    > :t 6.3
    6.3 :: Fractional a => a
    > :t (-10)
    (-10) :: Num a => a
    > :t (-10) + 6.3
    (-10) + 6.3 :: Fractional a => a
    ```

## 5.6 - Type Inference

- _Type inference_ is an algorithm for determining types of expressions.  Haskell's type inference is build on an extended version of the Damas-Hindley-Milner type system.

- Haskell infers the most generally applicable (polymorphic) type that is still correct - this is referred to as the _principal type_


## 5.7 - Asserting Types for Declarations

- Adding type signatures to functions is much better than relying on type inference.

- Declare type signature using `::` before function implementation:

    ```haskell
    triple :: Integer -> Integer
    triple x = x * 3
    ```

- It is possible to declare types locally using `let` or `where`, e.g.:

    ```haskell
    triple x = tripeItYo x
        where tripleItYo :: Integer -> Integer
                tripleItYo y = y * 3

    > :t triple
    triple :: Integer -> Integer
    ```
  Here, the assertion in the `where` clause narrowed the type of `triple` down from `Num a => a -> a` to `Integer -> Integer`.
