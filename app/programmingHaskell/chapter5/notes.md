# Chapter 5 - Types

## 5.3 - How to read type signatures

- Querying the type of a general number says it's a _constrained_ polymorphic number:

        > :t 13
        13 :: Num a => a

- We can give the number a _concrete_ type by declaring it explicitly:

        > let x = 13 :: Integer
        > :t x
        x :: Integer

- Can also query the type signatures of functions:

        > :t not
        not :: Bool -> Bool

- `(->)` is the type constructor for functions:

        > :i (->)
        data (->) a b  -- Defined in 'GHC.Prim'
        ... and more ...

- `->` is an infix operator that has two operands and associates to the right:

        > :t fst
        fst :: (a, b) -> a

    So `fst` is a function from a tuple `(a, b)` to an `a`.

- Compiler gives the least specific and most general type it can, often resulting in a _typeclass-constrained polymorphic type variable_:

        > :t (+)
        (+) :: Num a => a -> a -> a

    Here, we don't know the exact concrete type of `a` but we know it can only be a type that has the `Num` typeclass instance.

- A type signature might have multiple typeclass constraints on one or more of the variables:

        (Num a, Num b) => a -> b -> b
        (Ord a, Num a) => a -> a -> Ordering


# 5.4 - Currying

- All Haskell functions take one argument and return one result.

- Syntactic conventions conventions are used to construct _curried_ functions.

- Each `->` in a type signature represents one argument and one result, with the final type being the final result.

- Look at the type signature for addition:

        (+) :: Num a => a -> a -> a

  Since `->` is right-associative, this is equivalent to:

        (+) :: Num a => a -> (a -> a)

  i.e. `(+)` is a function which takes an `a` and returns (a function which takes an `a` and returns an `a`).

- _Partial application_ allows us to take a function and apply some of its arguments to get another function, e.g.:

        > let add10 = (+ 10)
        > add10 5
        15

- It is possible to _uncurry_ functions, replacing the curried version with an uncurried version that takes a tuple parameter containing all the arguments:

- _Sectioning_ refers to partial application of infix operators, with a special syntax:

        > let x = 5
        > let y = (2 ^)
        > let z = (^ 2)
        > y x
        32
        > z x
        25

- Sectioning also works with other non-arithmetic operators, but may need to use backticks to make prefix functions infix:

        > elem 9 [1..10]
        True
        > 9 `elem` [1..10]
        True
        > (`elem` [1..10]) 9
        True


