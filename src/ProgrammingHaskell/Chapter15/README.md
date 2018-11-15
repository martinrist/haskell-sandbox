# Chapter 15 - Monoid, Semigroup

## 15.3 - Monoid

- A _monoid_ is a set that is closed under a binary associative operation with an identity.

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


## 15.7 - Why bother?

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


## 15.9 - Different instance, same representation

- For some datatypes, the meaning of 'append', in the context of `Monoid` is less clear than for lists and numbers:

- 'Mappending' is best thought of not as a way of combining values, but as a way to condense any set of values to a summary value.

- `Bool` has two possible monoids - one of conjuction (represented by the `All` newtype) and one of disjunction (represented by `Andy`):

    ```haskell
    > import Data.Monoid

    > All True <> All True
    All {getAll = True}

    > All True <> All False
    All {getAll = False}

    > Any True <> Any Trye
    Any {getAny = True}

    > Any False <> Any True
    Any {getAny = True}
    ```

- `Maybe` has more than two possible monoid instances.

- `First` and `Last` are like boolean disjunction, but with preference for the first / last non-`Nothing` value:

    ```haskell
    > First Nothing <> First (Just 1) <> First (Just 2)
    First {getFirst = Just 1}

    > Last (Just 1) <> Last (Just 2) <> Last Nothing
    Last {getLast = Just 2}
    ```

- When declaring `Monoid` instances for types, beware of _orphan instances_, where the instance is declared in a different module from both the type and the typeclass:
    - Can easily lead to conflicting declarations.
    - To address if you own either the type or typeclass, put the instance in the same module as the thing you own.
    - If you own neither, define a newtype wrapping the original type, then define the instance in your module.


## 15.10 - Reusing algebras by asking for algebras

- In the following cases, you are getting a new `Monoid` for a larger type by reusing the `Monoid` instances of the component types:

    ```haskell
    instance Monoid b => Monoid (a -> b)
    instance (Monoid a, Monoid b) => Monoid (a, b)
    instance (Monoid a, Monoid b, Monoid c) => Monoid (a, b, c)
    ```

- The third variety of monoid for `Maybe a` combines the 'inner' a's (assuming they are monoids themselves:

    ```haskell
    instance Monoid a => Monoid (Maybe a) where
    mempty = Nada
    mappend Nothing  x        = x
    mappend x        Nothing  = x
    mappend (Just x) (Just y) = Just (x `mappend` y)

    > Just (Sum 1) <> Just (Sum 2)
    Just (Sum {getSum = 2})

    > Just [1, 2, 3] <> Nothing <> Just [4, 5, 6]
    Just [1, 2, 3, 4, 5, 6]

    > mempty :: Maybe String
    Nothing
    ```

## 15.12 - Using QuickCheck to test laws

- We can use QuickCheck to test monoid laws:

    ```haskell
    monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
    monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

    > quickCheck (monoidAssoc :: String -> String -> String -> Bool)

    monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
    monoidLeftIdentity a = (mempty <> a) == a

    > quickCheck (monoidLeftIdentity :: String -> Bool)

    monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
    monoidRightIdentity a = (a <> mempty) == a

    > quickCheck (monoidRightIdentity :: String -> Bool)
    ```


## 15.13 - Semigroup

- A _semigroup_ is a _monoid_ without the requirement for an identity:
    - Therefore, it is a set that is closed under an associative binary operation, and nothing else.
    - i.e. with just the `<>` operator, not `mempty`.
    - It's therefore a _weaker_ algebra than monoid.

- Defined in Haskell as `Data.Semigroup.Semigroup` (since GHC 8.0):

    ```haskell
    class Semigroup a where
        (<>) :: a -> a -> a

    -- associative law:
    (a <> b) <> c = a <> (b <> c)
    ```

- An example of a datatype that can't have a `Monoid` instance but can have a `Semigroup` instance is `Data.List.NonEmpty.NonEmpty`, which represents a non-empty list:

    ```haskell
    data NonEmpty a = a :| [a]
        deriving (Eq, Ord, Show)

    > import Data.List.NonEmpty
    > 1 :| [2, 3]
    1 :| [2, 3]

    > import Data.Semigroup
    > (1 :| [2, 3]) <> (4 :| [5, 6])
    1 :| [2, 3, 4, 5, 6]
    ```

- The _strength_ of an algebra corresponds to the number of operations it provides:
    - This in turn expands what you can do with an instance of that algebra without needing to know specifically what type you're working with.
    - However, this reduces the number of datatypes that can provide a law-abiding instance of that algebra.
    - For example, `Monoid` is stronger than `Semigroup`, which means you can do more with it, but types such as `NonEmpty` can't have `Monoid` instances.
