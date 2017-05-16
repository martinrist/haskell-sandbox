# Chapter 20 - Foldable

## 20.1 - Foldable

- `Foldable` encodes the more general concept of a _catamorphism_ and generalises it to many datatypes.

- A list is only one of many `Foldable` data types.



## 20.2 - The `Foldable` typeclass

- The definition of the `Foldable` typeclass is:

    ```haskell
    class Foldable (t :: * -> *) where
        foldMap :: Monoid m => (a -> m) -> t a -> m
        foldr :: (a -> b -> b) -> b -> t a -> t b
        fold :: Monoid m => t m -> m
        -- Other methods...
        {-# MINIMAL foldMap | foldr #-}
    ```

- The `MINIMAL` annotation tells us that a minimally-complete instance of `Foldable` requires an implementation of `foldMap` or `foldr`:
    - They can be defined in terms of each other.
    - All the other methods can be defined in terms of them.

- The `(t :: * -> *)` in the first line of the definition, says that `t` should be a _higher-kinded type_, of kind `* -> *`:
    - e.g. list, `Maybe` - anything that takes a single type argument.
    - Types that take more than one type argument (e.g. `Either` or tuples) will need to have their first type argument included as part of their structure (as for `Functor`).



## 20.3 - Monoids again

- Monoids are very important in folds, because folding requires an 'append' binary associative operation with an identity, which is what `Monoid` provides.

- This is explicit in the signatures for `foldMap` and `fold`:

    ```haskell
    fold :: Monoid m => t m -> m
    foldMap :: Monoid m => (a -> m) -> t a -> m
    ```

- `fold` combines elements inside a `Foldable` using the default `Monoid` instance defined for those elements:

    ```haskell
    > import Data.Foldable

    -- This doesn't work because Int has no Monoid
    > fold [1, 2, 3, 4, 5 :: Int]
    ... No instance for (Monoid Int) arising from a use of 'fold'

    > import Data.Monoid

    -- Sum Int has a monoid which adds
    > fold [1, 2, 3, 4, 5 :: Sum Int]
    Sum {getSum = 15}

    -- Product Int has a monoid which multiplies
    > fold [1, 2, 3, 4, 5 :: Product Int]
    Product {getProduct = 120}

    -- In this case, we don't need to be explicit about the Monoid used
    > fold ["hello", " world"]
    "hello world"
    ```

- `foldMap` takes a mapping function which maps each elmeent of the structure to a Monoid, then uses that Monoid for folding:

    ```haskell
    -- Sum is the data constructor for Sum Int
    > foldMap Sum [1, 2, 3, 4, 5 :: Int]
    Sum {getSum = 15}

    > foldMap Product [1, 2, 3, 4, 5 :: Int]
    Product {getProduct = 120}
    ```


## 20.4 - Some common `Foldable` instances

- `Foldable` instance for a simple `Identity` type is straightforward.  Here, folding is more a case of 'consuming' the data inside the structure, as opposed to 'reducing' multiple values:

    ```haskell
    data Identity a = Identity a

    instance Foldable Identity where
        foldMap f (Identity x) = f x
        -- These aren't needed but are shown for example:
        foldr f z (Identity x) = f x z
        foldl f z (Identity x) = f z x

    > fold $ Identity 5 :: Product Int
    Product {getProduct = 5}

    > fold $ Identity 5 :: Sum Int
    Sum {getSum = 5}
    ```

- In the case of `Maybe`, we have to handle the `Nothing` cases, which for `foldMap` we can use the monoid's `mempty`:

    ```haskell
    data Optional a = Nada | Yep a

    instance Foldable Optional where
        foldMap f Nada    = mempty
        foldMap f (Yep a) = f a

    > fold (Just 42) :: Sum Int
    Sum {getSum = 42}

    > fold Nothing :: Sum Int
    Sum {getSum = 0}

    > fold Nothing :: Product Int
    Product {getProduct = 1}
    ```


## 20.5 - Derived operations

- `Foldable` also includes other operations which can be derived in terms of the minimal functions:
    - Some of these (e.g. `length`) were defined for lists, but now are more general.

- `toList :: Foldable t => t a -> [a]` converts the type into a list:

    ```haskell
    > toList Nothing
    []

    > toList (Just 1)
    [1]

    -- This doesn't put the 1 in the list for the same reason that fmap doesn't apply a function to it
    > toList (1, 2)
    [2]
    ```

- `null :: Foldable t => t a -> Bool` tests whether the structure is 'empty':

    ```haskell
    > null (Just 1)
    False

    > null Nothing
    True
    ```

- `length :: Foldable t => t a -> Int` returns the size / length of a finite structure:

    ```haskell
    > length (Just 1)
    1

    > length Nothing
    0

    > length (1, 2)
    1

    > length [1, 2, 3, 4]
    4
    ```

- `elem :: (Eq a, Foldable t) => a -> t a -> Bool` tests whether an element occurs in the structure:

    ```haskell
    > elem 1 (Just 1)
    True

    > elem 1 (Just 2)
    False

    > elem 1 Nothing
    False

    > elem 1 [1, 2, 3]
    True
    ```

- `maximum :: (Ord a, Foldable t) => t a -> a` and `minimum` return the smallest and largest elements.

- `sum :: (Num a, Foldable t) => t a -> a` and `product` compute the sum and product of the numbers in a structure.
