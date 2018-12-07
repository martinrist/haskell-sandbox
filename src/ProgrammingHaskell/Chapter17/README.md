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

- We can also import `Control.Applicative` to give us access to some convenient functions that lift plain functions over applicative structure:

    ```haskell
    -- Note that `liftA` is just `fmap` over `Applicative`
    liftA  :: Applicative f => (a -> b)           -> f a -> f b
    liftA2 :: Applicative f => (a -> b -> c)      -> f a -> f b -> f c
    liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
    ````


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


### Maybe

- Here we have a function inside a `Maybe`, applied to a value inside a `Maybe`.

- Consider the case where we're validating inputs using various validation functions that take an input and return a `Maybe` that contains `Just input` or `Nothing`:

    ```haskell
    validateLength :: Int -> String -> Maybe String
    validateLength maxLen s =
        if (length s) > maxLen
        then Nothing
        else Just s
    ```

- Now let's say a `Person` has a `Name` (max length 25) and `Address` (max length 100), and we have functions to create each:

    ```haskell
    newtype Name = Name String deriving (Eq, Show)
    newtype Address = Address String deriving (Eq, Show)

    mkName :: String -> Maybe Name
    mkName s = fmap Name $ validateLength 25 s

    mkAddress :: String -> Maybe Address
    mkAddress s = fmap Address $ validateLength 100 s
    ```

- The question is how we compose `mkName` and `mkAddress` to create a smart constructor for `Person`, a product type of `Name` and `Address`:

    ```haskell
    data Person =
        Person Name Address
        deriving (Eq, Show)

    mkPerson :: String -> String -> Maybe Person

    -- Initial attempt requires us to manually pattern match on the various cases
    mkPerson :: String -> String -> Maybe Person
    mkPerson n a =
        case mkName n of
            Nothing -> Nothing
            Just n' ->
                case mkAddress a of
                    Nothing -> Nothing
                    Just a' -> Just $ Person n' a'
    ```

- A much better way is to `fmap` the `Person` constructor over `mkName n`, which gives us a `Maybe (Address -> Person)` - i.e. a function inside a `Maybe`.  This can then be applied using `<*>`:

    ```haskell
    mkPerson :: String -> String -> Maybe Person
    mkPerson n a = Person <$> mkName n <*> mkAddress a
    ```


## 17.6 - Applicative Laws

- _Identity_ - applying `id` wrapped in the `Applicative` to a value in the `Applicative` just returns that value:

    ```haskell
    pure id <*> v = v

    > pure id <*> [1..5]
    [1, 2, 3, 4, 5]

    > pure id <*> Just 2
    Just 2
    ```

- _Composition_ - composing functions then applying them should be the same as applying functions first:

    ```haskell
    pure (.) <*> u <*> v <*> w = u <*> (v <*> w)

    -- Composing first, then applying to [1, 2, 3]
    > pure (.) <*> [(+1)] <*> [(*2)] <*> [1, 2, 3]
    [3, 5, 7]

    -- Applying first
    > [(+1)] <*> ([(*2)] <*> [1, 2, 3])
    [3, 5, 7]
    ```

- _Homomorphism_ - the effect of applying a function embedded in a structure to a value in the structure should be the same as applying a function to a value without affecting any outside structure:

    ```haskell
    pure f <*> pure x = pure (f x)

    > pure (+1) <*> pure 1 :: Maybe Int
    Just 2

    > pure ((+1) 1) :: Maybe Int
    Just 2
    ```

- _Interchange_ - where `u` is a function embedded in some structure and `y` is a value:

    ```haskell
    u <*> pure y = pure ($ y) <*> u

    > Just (+2) <*> pure 3
    Just 5

    > pure ($ 3) <*> Just (+2)
    Just 5
    ```


## 17.7 - QuickChecking Applicative laws

- To avoid having to write own versions of properties for various laws, use the `checkers` library:

    ```haskell
    import Test.QuickCheck
    import Test.QuickCheck.Checkers
    import Test.QuickCheck.Classes

    data Bull = Fools | Twoo deriving (Eq, Show)

    instance Arbitrary Bull where
        arbitrary = frequency [ (1, return Fools, (1, return Twoo) ]

    -- Broken Monoid instance
    instance Monoid Bull where
        mempty = Fool
        mappend _ _ = Fools

    -- This is defined and needed by Test.QuickCheck.Checkers
    instance EqProp Bull where (=-=) = eq

    main :: IO ()
    -- Here we test monoid properties, and give an instance as a type hint
    main = quickBatch (monoid Twoo)
    ```

- If we want to test an existing `Applicative` instance, say for `[]`, we can do:

    ```haskell
    > quickBatch $ applicative [("a", "b", 1)]
    applicative:
        identity:      +++ OK, passed 500 tests.
        composition:   +++ OK, passed 500 tests.
        homomorphism:  +++ OK, passed 500 tests.
        interchange:   +++ OK, passed 500 tests.
        functor:       +++ OK, passed 500 tests.
    ```

- You can alternatively just use a typed _bottom_ to trigger the typeclass dispatch:

    ```haskell
    > quickBatch $ applicative (undefined :: [(String, String, Int)])
    ...
    ```


## 17.8 - ZipList Monoid

- The default monoid of lists is concatenation, but it's also possible to monoidally combine lists as parallel sequences:

    ```haskell
    instance Monoid a => Monoid (ZipList a) where
        mempty = pure mempty
        mappend = liftA2 mapped
    ```


## 17.9 - Either and Validation Applicative

- Specialisation of `f` to `Either e`:

    ```haskell
    -- f ~ Either e
    (<*>) ::        f (a -> b) ->        f a ->        f b
    (<*>) :: Either e (a -> b) -> Either e a -> Either e b

    pure :: a ->        f a
    pure :: a -> Either e a
    ```

- Although we can only have one `Functor` instance for a given datatype, we can have more than one valid `Monoid` (c.f. `Sum` and `Product` for `Integer`):
    - As a result, we can have more than one valid `Applicative` instance, which uses different `Monoid`s for the "monoidal" part of "monoidal functor".

- For example, `Either` short-circuits on the first failure:

    ```haskell
    > Right (+1) <*> Right 1
    Right 2

    > Right (+1) <*> Left ":("
    Left ":("

    > Left ":(" <*> Right 1
    Left ":("

    -- Note that the second value is lost
    > Left ":(" <*> Left "sadface.png"
    Left ":("
    ```

- `Validation` is an alternative to `Either` that has a different `Applicative`, which uses the `Monoid` of `err` to combine error values and retain all errors that have occurred:

    ```haskell
    data Validation err a =
        Failure err
      | Success a
      deriving (Eq, Show)

    > Success (+1) <*> Success 1
    Success 2

    > Success (+1) <*> Failure [StackOverflow]
    Failure [StackOverflow]

    > Failure [StackOverflow] <*> Success 1
    Failure [StackOverflow]

    -- Uses the [] monoid to combine the errors
    > Failure [StackOverflow] <*> Failure [UnknownError]
    Failure [StackOverflow, UnknownError]
    ```
