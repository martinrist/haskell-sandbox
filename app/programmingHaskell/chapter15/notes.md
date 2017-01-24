# Chapter 15 - Monoid, Semigroup

## 15.3 - Monoid

- A _monoid_ is a binary associative operation with an identity.

    ```haskell
    -- `mappend` is the binary operation
    > mappend [1..5] [4..6]
    [1, 2, 3, 4, 5, 6]

    > mappend "foo" "bar"
    "foobar"

    -- `mempty` is the identity
    > mappend [1..5] []
    [1, 2, 3, 4, 5]

    > mappend [] [1..5]
    [1, 2, 3, 4, 5]
    ```


## 15.4 - How Monoid is defined in Haskell

- Defined by the `Monoid` typeclass:

    ```haskell
    > :i Monoid
    class Monoid a where
        mempty :: a
        mappend :: a -> a -> a
        mconcat :: [a] -> a
    ```

- `mconcat` is defined as:

    ```haskell
    mconcat :: [a] -> a
    mconcat = foldr mappend mempty
    ```

- There are additional laws, but these aren't defined by the typeclass - it's the responsibility of typeclass instances to ensure these rules are met by the instance implementations.


## 15.5 - Examples of `Monoid`

- A common type with an instance of `Monoid` is `[a]`:

    ```haskell
    > mappend [1, 2, 3] [4, 5, 6]
    [1, 2, 3, 4, 5, 6]

    > mconcat [[1..3], [4..6]]
    [1, 2, 3, 4, 5, 6]
    ```

- The definition of the `Monoid` instance for lists is simple:

    ```haskell
    instance Monoid [a] where
        mempty = []
        mappend = (++)
    ```


## 15.6 - Why `Integer` doesn't have a `Monoid`

- The type `Integer` doesn't have a `Monoid` instance, because the monoid implementation for numbers could either be summation or multiplication:
    - Both these operations are monoidal.
    - However, each type should only have one instance for a given typeclass.

- To resolve this, `Sum` and `Product` newtype wrappers have declared in `Data.Monoid`.  Each of these newtypes has the relevant monoid:

    ```haskell
    > import Data.Monoid
    > :i Sum
    newtype Sum a = Sum {getSum :: a}
    ...

    > :i Product
    newtype Product a = Product {getProduct :: a}

    > mappend (Sum 2) (Sum 4)
    Sum {getSum = 6}
    > mempty :: Sum Int
    Sum {getSum = 0}

    > mappend (Product 2) (Product 4)
    Product {getProduct = 8}
    > mempty :: Product Int
    Product {getProduct = 1}
    ```

- This is a general pattern - if we need multiple instances for a given typeclass, create newtype wrappers for each instance and attach the instance to the newtype.

- `mappend` only takes two arguments.  To use more, either use parentheses, or use `mappend`'s infix variant, `<>`:

    ```haskell
    > (Sum 1) <> (Sum 2) <> (Sum 3) <> (Sum 4)
    Sum {getSum = 10}
    ```

- Alternatively, put the `Sum`s in a list, then `mconcat` them:

    ```haskell
    > mconcat [Sum 1, Sum 2, Sum 3, Sum 4]
    Sum {getSum = 10}
    ```


## 15.7 - WHy bother?

- Common uses of monoids are to structure and describe common modes of processing data, e.g.:
    - Incrementally processing a large dataset
    - Rolling up data into aggregations (cf. summation)
    - Using identity to provide extra values when doing 'divide-and-conquer' algorithms that require padding out to an even number / power of two.

- _Abelian monoids_ or _commutative monoids_ are monoids where the order of combination does not matter, i.e.:

    ```haskell
    mappend x y == mappend y x
    ```

- Monoids are even more strongly associated with folding or catamorphisms:

    ```haskell
    > foldr mappend mempty ([2, 4, 6] :: [Product Int])
    Product {getProduct = 48}

    > foldr mappend mempty ([2, 4, 6] :: [Sum Int])
    Sum {getSum = 12}
    ```


## 15.8 - Laws

- Algebras are defined by their laws, and derive their usefulness principally from those laws.

- Monoid laws are:

    ```haskell
    -- left identity
    mappend mempty x == x

    -- right identity
    mappend x mempty == x

    -- associativity
    mappend x (mappend y z) == mappend (mappend x y) z

    -- concatenation
    mconcat = foldr mappend mempty
    ```

- The above laws apply regardless of which monoids you're working with, `Sum`, `Product`, `[a]` etc.
