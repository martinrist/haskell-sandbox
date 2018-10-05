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


## 25.7 - Monad Transformers

- Recall from earlier that a _Monad transformer_ is a type constructor that takes a Monad as an argument and returns a Monad as a result.

- When attempting to 'compose' two Monads, we saw that the fundamental problem is trying to join the two unknown Monads:
    - To get around this, we need to reduce the polymorphism and get concrete information about one of the Monads we're working with.
    - The other Monad remains polymorphic as a variable type argument to our type constructor.

- Let's think about how we get a bind operation over a type like `IO (Reader String [a])` where the Monad instances are `IO`, `Reader` and `[]`.

- We could make one-off types for each combination of two Monads, but that won't scale:

    ```haskell
    newtype MaybeIO a =
        MaybeIO { runMaybeIO :: IO (Maybe a) }

    newtype MaybeList a =
        MaybeList { runMaybeList :: [Maybe a] )

    newtype IOList a =
        IOList { runIoList :: IO [a] }
    ```

- It turns out, however, that we can get a Monad for _two_ types, provided we know what _one_ of them is - this is what Monad transformers do.


## 25.8 - `IdentityT`

- `IdentityT` is the simplest of the Monad transformers:
    - Just like `Identity` helps start to understnad the essence of `Functor`, `Applicative` and `Monad`.

- The new `IdentityT` data type:

    ```haskell
    -- The original Identity type
    newtype Identity a =
        Identity { runIdentity :: a }
        deriving (Eq, Show)

    -- The IdentityT monad transformer
    newtype IdentityT f a =
        IdentityT { runIdentityT :: f a }
        deriving (Eq, Show)
    ```

- `Functor` and `Applicative` instances for `IdentityT` are straightforward:

    ```haskell
    instance Functor m => Functor (IdentityT m) where
        fmap f (IdentityT fa) = IdentityT (fmap f fa)

    instance Applicative m => Applicative (IdentityT m) where
        pure x = IdentityT (pure x)
        (IdentityT fab) <*> (IdentityT fa) =
            IdentityT (fab <*> fa)
    ```

- The Monad instance is where we have to use concrete type information from `IdentityT` (specifically the call to `runIdentityT`) to make the types fit:

    ```haskell
    instance Monad m => Monad (IdentityT m) where
        return = pure
        (IdentityT ma) >>= f =
            IdentityT $ ma >>= runIdentityT . f
    ```

- Looking in detail at the bind implementation:

    ```haskell
    (IdentityT ma) >>= f   = IdentityT $ ma  >>= runIdentityT . f

    [     1      ] [2] [3]   [   8   ]   [4] [5] [    7     ]   [6]
    ```

- Taking each component in turn:
    1. Pattern-match and unpack the `m a` value of `IdentityT m a`.  The type of `ma` is thus `m a`.
    2. We are implementing a bind of type `(>>=) :: IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b`.
    3. `f` is the function we're binding over, of type `f :: a -> IdentityT m b`.
    4. This is the `ma` we unpacked out of [1].  It is of type `m a`.
    5. This bind is the one for the Monad `m`.  It is of type `(>>=) :: m a -> (a -> m b) -> m b`
    6. This is the same `f :: a -> IdentityT m b` as [3] on the left hand side.
    7. The bind [5] needs a second argument of type `a -> m b`, so we need to compose `f` with `runIdentity :: IdentityT m b -> m b` to make the types line up.
    8. Finally, to make the bind [2] return `IdentityT m b`, we need to wrap the whole result from [4] - [6] in an `IdentityT`.

- Once we have the Monad instance in place, we can use it as follows:

    ```haskell
    > IdentityT [1, 2, 3] >>= return . (+1)
    IdentityT { runIdentityT = [2, 3, 4] }

    > IdentityT [1, 2, 3] >>= (\x -> IdentityT [x, x * 2])
    IdentityT {runIdentityT = [1,2,2,4,3,6]}
    ```
