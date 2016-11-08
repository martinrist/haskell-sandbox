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

