# Chapter 16 - Functor

## 16.2 - `Functor` definition

- A _functor_ is a way of applying a function over or around some structure, when we don't want to alter the structure.

- Implemented with the `Functor` typeclass, which defines the `fmap` function:

    ```haskell
    class Functor f where
        fmap :: (a -> b) -> f a -> f b
    ```


## 16.3 - Examples of `fmap`

- `fmap` for lists is the same as `map`:

    ```haskell
    > map (\x -> x + 3) [1..6]
    [4, 5, 6, 7, 8, 9]

    > fmap (\x -> x + 3) [1..6]
    [4, 5, 6, 7, 8, 9]
    ```

- However, `fmap` works over other functors, unlike `map`:

    ```haskell
    > map (+1) (Just 1)
    ... type error

    > fmap (+1) (Just 1)
    Just 2

    > fmap (+1) (2, 3)
    (2, 4)
    ```


## 16.4 - The _f_ in `Functor f`

- The _f_ in the typeclass definition for `Functor` must be the same across the definition, and it must refer to to a type that implements the typeclass.

- _f_ must have the kind `* -> *`:
    - i.e. it is a _higher-kinded type_, that is awaiting application to a type constant of kind `*`.
    - We know this because every argument to the type constructor of `->` must be of kind `*` (`:k (->)` is `* -> * -> *`).
    - Therefore, in the signature of `fmap` both the `f a` and `f b` must be of kind `*`.
    - Therefore, the `f` must be of kind `* -> *`.

- There is also an infix operator `<$>` for `fmap`:

    ```haskell
    > :t (<$>)
    <$> :: Functor f => (a -> b) -> f a -> f b
    ```

- Note the similarity between `<$>` and `$` - this is because `fmap / <$>` is like function application (`$`) 'lifted' over the functor.

- Here's an example of implementing `Functor`:

    ```haskell
    data FixMePls a =
        FixMe
      | Pls a
      deriving (Eq, show)

    instance Functor FixMePls where
        fmap _ FixMe   = FixMe
        fmap f (Pls a) = Pls (f a)
    ```


## 16.5 - `Functor` laws

- Like `Monoid`, instances of the `Functor` typeclass should satisfy laws:

    ```haskell
    -- Identity - mapping `id` over a Functor should leave it unchanged
    fmap id == id

    -- Composability - mapping the composition is the same as mapping each function in turn
    fmap (f . g) == fmap f . fmap g
    ```


## 16.6 - Examples of broken `Functor`s

- A simple functor that breaks the identity law:

    ```haskell
    data WhoCares a =
        ItDoesnt
      | Matter a
      | WhatThisIsCalled
    deriving (Eq, Show)

    instance Functor WhoCares where
        fmap _ ItDoesnt         = WhatThisIsCalled
        fmap _ WhatThisIsCalled = ItDoesnt
        fmap f (Matter a)       = Matter (f a)

    > fmap id ItDoesnt == id ItDoesnt
    False
    ```

- Example of breaking composition, because we mess with part of the structure (by incrementing the `n`:

    ```haskell
    data CountingBad a =
        Heisenberg Int a
        deriving (Eq, Show)

    instance Functor CountingBad where
        fmap f (Heisenberg n a) = Heisenberg (n+1) (f a)

    > let f = (++ " Jesse")
    > let g = (++ " lol")
    > let u = Heisenberg 0 "Uncle"

    > fmap (f . g) u
    Heisenberg 1 "Uncle lol Jesse"

    > fmap f . fmap g $ u
    Heisenberg 2 "Uncle lol Jesse" 
    ```

## 16.7 - Commonly used functors

- The functor of functions just does composition:

    ```haskell
    > let tossEmOne = fmap (+1) negate
    > tossEmOne 10
    -9
    > tossEmOne (-10)
    11
    ```

- We can have functors within functors, and can compose `fmap` to get into the 'inner' functors:

    ```haskell
    -- lms is a [Maybe String]
    > let lms = [Just "Ave", Nothing, Just "woohoo"]

    > const 'p' lms
    'p'

    -- Outer functor is the list
    > fmap (const 'p') lms
    "ppp"

    -- Composing two `fmap`s 'digs' into the inner 'Maybe'
    > (fmap . fmap) (const 'p') lms
    [Just 'p', Nothing, Just 'p']

    -- And three `fmap`s gets into the String
    > (fmap . fmap . fmap) (const 'p') lms
    [Just "ppp", Nothing, Just "pppppp"]
    ```


## 16.8 - Transforming the unapplied type argument

- When we `fmap` over a tuple, only the second arguent is transformed:

    ```haskell
    > fmap (+1) (1, 2)
    (1, 3)
    ```

- Similarly using `fmap` over an `Either` value only transforms the `Right` value:

    ```haskell
    > fmap (+1) (Right 1)
    Right 2

    > fmap (+1) (Left 1)
    Left 1
    ```

- We need to work out how to write `Functor` instances for the following types, both of which have kind `* -> * -> *`:

    ```haskell
    -- Like `(,)`
    data Two a b =
        Two a b
        deriving (Eq, Show)

    -- Like `Either`
    data Or a b =
        First a
      | Second b
      deriving (Eq, Show)
    ```

- To do this, we need to reduce the kindedness by applying a type variable that represents a type constant.  In the implementation we then need to leave this first argument alone, because it's 'part of the structure':

    ```haskell
    instance Functor (Two a) where
        fmap f (Two a b) = Two $ a (f b)

    instance Functor (Or a) where
        fmap _ (First a)  = First a
        fmap f (Second b) = Second (f b)
    ```


## 16.9 - QuickChecking `Functor` instances

- Recall the `Functor` laws:

    ```haskell
    fmap id      = id
    fmap (f . g) = (fmap f) . (fmap g)
    ```

- These can be written as the following QuickCheck properties:

    ```haskell
    functorIdentity :: (Functor f, Eq (f a)) =>
                            f a
                         -> Bool
    functorIdentity f = fmap id f == f

    functorCompose :: (Eq (f c), Functor f) =>
                            (a -> b)
                         -> (b -> c)
                         -> f a
                         -> Bool
    functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)
    ```

- We can then test these with concrete instances:

    ```haskell
    > quickCheck $ \x -> functorIdentity (x :: [Int])

    > let li x = functorCompose (+1) (*2) (x :: [Int])
    > quickCheck li
    ```


## 16.11 - Ignoreing possibilities

- The `Functor` instances for `Maybe` and `Either` are handy for when you intend to ignore the 'left' cases, which are typically the error or failure cases.

- `fmap` doesn't touch these cases, so we can map functions right to the values that you intend to work with, and ignore the failure cases.

- For the `Maybe` type, consider ordinary pattern matching:

    ```haskell
    incIfJust :: Num a => Maybe a -> Maybe a
    incIfJust (Just n) => Just $ n + 1
    incIfJust Nothing = Nothing

    showIfJust :: Show a => Maybe a -> Maybe String
    showIfJust (Just s) = Just $ show s
    showIfJust Nothing = Nothing
    ```

- Because of the instance of `Functor` for `Maybe` (which skips over `Nothing`), these are equivalent to:

    ```haskell
    incMaybe :: Num a => Maybe a -> Maybe a
    incMaybe m = fmap (+1) m

    showMaybe :: Show a => Maybe a -> Maybe String
    showMaybe s = fmap show s
    ```

- Via eta reduction, we can rewrite these as:

    ```haskell
    incMaybe :: Num a => Maybe a -> Maybe a
    incMaybe = fmap (+1)

    showMaybe :: Show a => Maybe a -> Maybe String
    showMaybe = fmap show
    ```

- In fact, these don't use anything specific to `Maybe`, so they can be more generally rewritten as:

    ```haskell
    liftedInc :: (Functor f, Num a) => f a -> f a
    liftedInc = fmap (+1)

    liftedShow :: (Functor f, Show a) => f a -> f String
    liftedShow = fmap show
    ```

- Similarly, with `Either`, we can do the same, simplifications, leading to the same end result:

    ```haskell
    incIfRight :: Num a => Either e a -> Either e a
    incIfRight (Right n) = Right $ n + 1
    incIfRight (Left  e) = Left e

    showIfRight :: Show a => Either e a -> Either e String
    showIfRight (Right s) = Right $ show s
    showIfRight (Left  e) = Left e

    -- Equivalent to
    incEither :: Num a => Either e a -> Either e a
    incEither = fmap (+1)

    showEither :: Show a => Either e a -> Either e String
    showEither = fmap show

    -- Equivalent to
    liftedInc :: (Functor f, Num a) => f a -> f a
    liftedInc = fmap (+1)

    liftedShow :: (Functor f, Show a) => f a -> f String
    liftedShow = fmap show
    ```


