# Chapter 25 - Composing Types

## 25.1 - Composing types

- Functors and applicatives are both closed under composition:
    - i.e. you can compose two functors / applicatives and get back another functor / applicative.
    - The same is not true for Monads - the composition of two Monads is not necessarily a Monad.

- However, composing Monads is often desirable:
    - Different Monads allow us to work with different effects.
    - e.g. performing IO actions while also dealing with failure using `IO` and `Maybe`.

- A _Monad Transformer_ is a variant of an ordinary type that takes an additional type argument:
    - This type argument is assumed to have a `Monad` instance.
    - e.g. `MaybeT :: m (Maybe a) -> MaybeT m a` is the transformer variant of the `Maybe` type.


## 25.2 - Common functions as types

- We'll start with `newtype`s that correspond to some very basic functions (`id` and `(.)`).

- Consider the `Identity` newtype first, which corresponds to `id`:

    ```haskell
    newtype Identity a =
        Identity { runIdentity :: a }
        deriving (Eq, Show)
    ```

- Note that monad transformers are usually written using `newtype` rather than `data`, to avoid overhead:
    - Using `newtype` means they have a representation identical to the type they contain.

- Next, consider the `Compose` newtype, that corresponds to function composition - `(.)`:

    ```haskell
    newtype Compose f g a =
        Compose { getCompose :: f (g a) }
        deriving (Eq, Show)
    ```

- In `Compose`, `f` and `g` are type constructors themselves, whereas `a` is a concrete type, as can be seen from the kind of `Compose`:

    ```haskell
    > :k Compose
    Compose :: (* -> *) -> (* -> *) -> * -> *
    ```

- With specific type constructors for `f` and `g`:

    ```haskell
    > :t Compose [Just 1 :: Int, Nothing]
    Compose [Just (1 :: Int), Nothing] :: Compose [] Maybe Int

    -- The other way around
    > :t Compose $ Just [1 :: Int, 2, 3]
    Compose $ Just [1 :: Int, 2, 3] :: Compose Maybe [] Int
    ```


## 25.3 - `Functor` instances for `Identity` and `Compose`

- Lifting a function over `Identity` is easy:

    ```haskell
    instance Functor Identity where
        fmap f (Identity a) = Identity (f a)
    ```

- With `Compose` it's slightly more complex - we need to lift the function over both `f` and `g`, so we compose two `fmap`s:

    ```haskell
    instance (Functor f, Functor g) =>
             Functor (Compose f g) where
        fmap f (Compose fga) =
            Compose $ (fmap . fmap) f fga

    > fmap (+1) $ Compose [Just 1, Nothing, Just 2]
    Compose { getCompose = [Just 2, Nothing, Just 3]}
    ```


## 25.4 - `Applicative` instances for `Identity` and `Compose`

- It's also possible to write a trivial `Applicative` instance for `Identity`:

    ```haskell
    instance Applicative Identity where
        pure = Identity
        (Identity f) <*> (Identity a) = Identity $ f a
    ```

- An instance for `Compose` is a bit harder, but relies on `f` and `g` having `Applicative` instances which can be used (also using `InstanceSigs` to make the types clearer:

    ```haskell
    instance (Applicative f, Applicative g) =>
             Applicative (Compose f g) where

        pure :: a -> Compose f g a
        pure a = Compose $ pure (pure a)

        (<*>) :: Compose f g (a -> b)
              -> Compose f g a
              -> Compose f g b
        (Compose fgab) <*> (Compose fga) =
            Compose $ (<*>) <$> fgab <*> fga
    ```


## 25.5 - Attempting a `Monad` instance

- There's no problem composing two arbitrary datatypes that have a `Monad` instance:
    - However, the result is not necessarily a monad.
    - This is because of a 'lack of information'.

- Both types in `Compose f g` are polymorphic, so when you try to write bind for the monad, you're trying to combine two polymorphic binds into a single combined bind:

    ```haskell
    instance (Monad f, Monad g) => Monad (Compose f g) where
        return = pure

        (>>=) :: Compose f g a
              -> (a -> Compose f g b)
              -> Compose f g b
        -- This isn't possible
        (>>=) = ...
    ```

- The problem with combining the binds is:

    ```haskell
    -- Given the following:
    (>>=) :: Monad f => f a (a -> f b) -> f b
    (>>=) :: Monad g => g a (a -> g b) -> g b

    -- We're effectively trying to write:
    (>>=) :: (Monad f, Monad g) => f (g a) -> (a -> f (g b)) -> f (g b)

    -- Or, equivalently:
    join :: (Monad f, Monad g) => f (g (f (g a))) -> f (g a)

    -- And it's not possible to `join` that final `f` and `g`.
    ```
