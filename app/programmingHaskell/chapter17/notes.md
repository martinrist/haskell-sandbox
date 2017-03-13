# Chapter 17 - Applicative

## 17.1 - Introduction

- `Applicative` is a _monoidal functor_ - like a combination of `Monoid` and `Functor`:
    - `Monoid` gives us a means of mashing two values of the same type together.
    - `Functor` gives us a means of applying a function _over_ some structure (e.g. list).

- `Applicative` combines these two:
    - It allows us to have function application 'lifted' over structure (like `Functor`)
    - However, with `Applicative` the function being applied is also embedded in some structure.

- Because both the function _and_ the value its applied to are both embedded in some structure, we have to combine those structures, which is where the `Monoid` part comes in.


## 17.2 - Defining `Applicative`

- The typeclass declaration for `Applicative` is:

    ```haskell
    class Functor f => Applicative f where
        pure  :: a -> f a
        (<*>) :: f (a -> b) -> f a -> f b
    ```

- Note that the `f` that represents the structure is constrained by the `Functor` typeclass:
    - i.e. for the `f` to have an instance of `Applicative` it must also have a `Functor` typeclass.

- `pure` just embeds something into the applicative structure:

    ```haskell
    > pure 1 :: [Int]
    [1]

    > pure 1 :: Maybe Int
    Just 1
    ```

- `<*>`, also called _apply_ takes functions embedded in structure and applies those to values embedded in structure:

    ```haskell
    > :t (<*>)
    (<*>) :: Applicative f => f (a -> b) -> f a -> f b
    ```


## 17.3 - `Functor` vs `Applicative`

- Comparing the types of `<*>` and `fmap` (or `<$>`), we can see the difference between them is that `<$>` wraps the funciton in a `f`:

    ```haskell
    (<$>) :: Functor f     =>   (a -> b) -> f a -> f b
    (<*>) :: Applicative f => f (a -> b) -> f a -> f b
    ```

- They are 'linked' in that `f <$> x = pure f <*> x`:

    ```haskell
    > (+1) <$> [1, 2, 3]
    [2, 3, 4]

    > pure (+1) <*> [1, 2, 3]
    [2, 3, 4]
    ```


## 17.4 - Applicative functors are monoidal functors

- In the type for `<*>`, consider separating the _structure_ parts from the _function_ parts:

    ```haskell
    (<*>) :: Applicative f => f (a -> b) -> f a -> f b

              Structure ->    f             f      f
              Function  ->    (a -> b)      a      b
    ```

- Provided the `f` is a type with a `Monoid` instance, then we can make the 'top' line work.  Then the bottom line is just function application:

    ```haskell
              mappend ::      f             f      f
              $       ::      (a -> b)      a      b

              (<*>)   ::      f (a -> b) -> f a -> f b
    ```

- Examples:

    ```haskell
    > [(*2), (*3)] <*> [4, 5]
    [8, 10 , 12, 15]

    > Just (*2) <*> Just 2
    Just 4

    > Just (*2) <*> Nothing
    Nothing

    > Nothing <*> Just 2
    Nothing
    ```

- Recall from earlier that the `Functor` instance for the two-tuple ignores the first value inside the tuple:

    ```haskell
    > (+1) <$> ("bar", 0)
    ("bar", 1)
    ```

- In contrast, the `Applicative` instance for the two-tuple `(a, b)` requires the `a` to have a `Monoid` instance, which it uses to 'combine' the first values in the tuple:

    ```haskell
    > :i (,)
    ...
    instance Monoid a => Applicative ((,) a)
    ...

    -- Now the first argument is in a two-tuple
    > ("foo", (+1)) <*> ("bar", 0)
    ("foobar", 1)
    -- Note how "foo" and "bar" are combined using `String`'s `Monoid` instance
    ```

- Other examples - note how the `a` values in the tuples are combined:

    ```haskell
    > ((Sum 2), (+1)) <*> ((Sum 3), 0)
    (Sum {getSum = 5}, 1)

    > ((All True), (+1)) <*> ((All False), 0)
    (All {getAll = False}, 1)
    ```

- It's instructive to compare the `Monoid` and `Applicative` instances for the two-tuple to see the similarities:

    ```haskell
    instance (Monoid a, Monoid b) => Monoid (a, b) where
        mempty = (mempty, mempty)
        (a, b) `mappend` (a', b') = (a `mappend` a', b `mappend` b')

    instance Monoid a => Applicative ((,) a) where
        pure x = (mempty, x)
        (u, f) <*> (v, x) = (u `mappend` v, f x)
    ```


## 17.5 - Applicative in use

### List

- The `List` applicative maps a plurality of functions over a plurality of values, using a 'cartesian product'-style combination.


### Maybe

- The `Maybe` applicative maps a possible function over a possible value.  To see the use of this, consider looking up values using the `lookup` function:

    ```haskell
    > :t lookup
    lookup :: Eq a => a -> [(a, b)] -> Maybe b

    > let m = [(1, "foo"), (2, "bar"), (3, "blort")]
    > lookup 1 m
    Just "foo"

    > lookup 4 m
    Nothing
    ```

- Say we want to look up two values and concatenate them.  The pattern is:

    ```haskell
    > (++) <$> lookup 1 m <*> lookup 2 m
    Just "foobar"

    > (++) <$> lookup 4 m <*> lookup 2 m
    Nothing

    > (++) <$> lookup 1 m <*> lookup 4 m
    Nothing
    ```

- This works because we first map `(++)` over `lookup 1 m`, which is a `Maybe String`, giving a `Maybe (String -> String)`:

    ```haskell
    > :t (++) <$> lookup 1 m
    (++) <$> lookup 1 m :: Maybe ([Char] -> [Char])
    ```

- Then we use `<*>` to apply the `Maybe (String -> String)` to the `Maybe String` from `lookup 2 m`.

- Alternatively, we can use `liftA2`, which takes a binary function and lifts it over `f`:

    ```haskell
    > :t liftA2
    liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c

    > liftA2 (++) (lookup 1 m) (lookup 2 m)
    Just "foobar"
    ```

### IO

- The applicative context can also be `IO`:

    ```haskell
    > :t getLine
    getLine :: IO String

    > (++) <$> getLine <*> getLine
    > foo
    > bar
    "foobar"
    ```

### Identity

- The `Identity` type is a way of introducing structure without changing the semantics of anything that's happening:

    ```haskell
    newtype Identity a = Identity a
        deriving (Eq, Ord, Show)
    ```

- We might introduce this extra structure to lift a function over the `Identity`, rather than the thing it contains:

    ```haskell
    -- This version does the usual 'cartesian product' thing
    > const <$> [1, 2, 3] <*> [9, 9, 9]
    [1, 1, 1, 2, 2, 2, 3, 3, 3]

    -- Wrapping each list in an Identity means that `const` is lifted
    -- over the Identity, not the lists:
    > const <$> Identity [1, 2, 3] <*> Identity [9, 9, 9]
    Identity [1, 2, 3]
    ```


### Constant

- `Constant` is similar to `Identity` (in that it provides structure) but also acts like `const`.

- It sort of throws away a function application, because it takes two arguments, but one of them just gets discarded:

    ```haskell
    newtype Constant a b =
        Constant { getConstant :: a }
        deriving (Eq, Ord, Show)
    ```

- Consider specialising the types for `(<*>)`, by setting `f ~ Constant e`:

    ```haskell
    (<*>) ::          f (a -> b) ->          f a ->          f b
    (<*>) :: Constant e (a -> b) -> Constant e a -> Constant e b

    pure :: a ->          f a
    pure :: a -> Constant e a
    ```


