# Chapter 26 - Monad Transformers

## 26.2 - `MaybeT`

- `MaybeT` is a bit more complicated than `IdentityT`:

    ```haskell
    > import Control.Monad.Trans.Maybe
    > :i MaybeT
    newtype MaybeT (m :: * -> *) a =
        MaybeT { runMaybeT :: m (Maybe a) }
    ```

- `MaybeT` is very similar to the `Compose` type we saw in [chapter 25](../chapter25/notes.md), so we can reuse what we did there for the `Functor` and `Applicative` instances:

    ```haskell
    -- `Functor` for `Compose`
    instance (Functor f, Functor g) =>
              Functor (Compose f g) where
        fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

    -- `Functor` for `MaybeT`
    instance (Functor m) => Functor (MaybeT m) where
        fmap f (MaybeT ma) =
            MaybeT $ (fmap . fmap) f ma

    -- `Applicative` for `Compose`
    instance (Applicative f, Applicative g) =>
              Applicative (Compose f g) where
        pure x = Compose (pure (pure x))
        Compose f <*> Compose x = Compose $ (<*>) <$> f <*> x)

    -- `Applicative` for `MaybeT`
    instance (Applicative m) => Applicative (MaybeT m) where
        pure x = MaybeT (pure (pure x))
        (MaybeT fab) <*> (MaybeT mma) =
            MaybeT $ (<*>) <$> fab <*> mma
    ```

- Here's the `Monad` instance for `MaybeT`, with annotations for the types:

    ```haskell
    instance (Monad m) => Monad (MaybeT m) where
        return = pure

        (>>=) :: MaybeT m a
              -> (a -> MaybeT m b)
              -> MaybeT m b

        (MaybeT ma) >>= f   =
    --          [2]     [3]
            MaybeT $ do
    --      [  1 ]
                -- ma :: m (Maybe a)
                --  v :: Maybe a
                v <- ma
    --            [4]
                case v of
    --              [5]
                    Nothing -> return Nothing
    --              [           6           ]
                    --              y :: a
                    --              f :: a -> MaybeT m b
                    --            f y :: Maybe T m b
                    -- runMaybe (f y) :: m (Maybe b)
                    Just y  -> runMaybeT (f y)
    --                  [7]       [8]
    ```


- Notes:
    1. We have to return a `MaybeT m b` at the end, so the `do` block has the `MaybeT` data constructor in front of it.  The final value of our `do` block should be of type `m (Maybe b)`, because we're going to apply `MaybeT :: m (MaybeT b) -> MaybeT m b` to it to get the `MaybeT m b` we're looking for.
    2. `ma :: m (Maybe a)`, since we destructured it in the pattern match.
    3. `f :: a -> MaybeT m b`.
    4. `v <- ma` extracts the `Maybe a` value out of `ma :: m (Maybe a)`.
    5. `v :: Maybe a`, so we match on it being either `Nothing` or `Just y`.
    6. If `v` is `Nothing`, we return `Nothing`, but it needs to be wrapped back up in `m`, using `m`'s `return` implementation - it then returns a `m (Maybe b)`, as required to pass into `MaybeT` in step 1.
    7. If `v` is `Just y`, then `y :: a` and `f y :: MaybeT m b`.
    8. Finally, calling `runMaybeT` on `f y` turns the `MaybeT m b` into `m (Maybe b)`, which is what we pass into `MaybeT` in step 1 to finally get a `MaybeTWe have to get the `m b` value out of `f y`, which we can do by just calling `runMaybeT`.


## 26.3 - `EitherT`

- The instances for `EitherT` are similar to those for `MaybeT`:

    ```haskell
    newtype EitherT e m a =
        EitherT { runEitherT :: m (Either e a) }

    instance Functor m => Functor (EitherT e m) where
        fmap f (EitherT meea) =
            EitherT $ (fmap . fmap) f meea

    instance Applicative m => Applicative (EitherT e m) where
        pure x = EitherT (pure (pure x))
        (EitherT efab) <*> (EitherT ea) =
            EitherT $ (<*>) <$> efab <*> ea

    instance Monad m => Monad (EitherT e m) where
        return = pure

        (>>=) :: EitherT e m a
              -> (a -> EitherT e m b)
              -> EitherT e m b

        (EitherT ema) >>= f =
            EitherT $ do
                v <- ema
                case v of
                    Left e  -> return $ Left e
                    Right b -> runEitherT (f b)
    ```



## 26.4 - `ReaderT`

- `ReaderT` is like `Reader`, but we're generating additional structure in the return type of the function:

    ```haskell
    newtype ReaderT r m a =
        ReaderT { runReaderT :: r -> m a }

- As with `Reader`, the value inside the `ReaderT` is a function.  The first argument is part of the structure we'll have to bind over.

- Instances for `Functor` and `Applicative` are again similar to `IdentityT` and `MaybeT`:

    ```haskell
    -- Functor for ReaderT
    instance Functor m => Functor (ReaderT r m) where
        fmap f (ReaderT rma) =
            ReaderT $ (fmap . fmap) f rma

    -- Applicative for ReaderT
    instance Applicative m => Applicative (ReaderT r m) where
        pure a = ReaderT $ pure (pure a)

        (ReaderT fmab) <*> (ReaderT rma) =
            ReaderT $ (<*>) <$> fmab <*> rma
    ```

- As before, the `Monad` instance is slightly more complex:

    ```haskell
    -- Monad for ReaderT
    instance Monad m => Monad (ReaderT r m) where
        return = pure

        (>>=) :: ReaderT r m a
              -> (a -> ReaderT r m b)
              -> ReaderT r m b

        (ReaderT rma) >>= f =
            ReaderT $ \r -> do
            --       [1]
                a <- rma r
            --   [3] [ 2 ]
                runReaderT (f a) r
            --  [   5    ] [ 4 ] [6]
    ```

- In the above:
    1. The type of the value in `ReaderT` is a function (`r -> ma`), so the act of binding `f` over a `ReaderT` must be a function awaiting an `r` argument.
    2. We've pattern-matched `rma :: r -> m a` out of the `ReaderT r m a` argument, so it's a function that needs to be applied to `r`.
    3. Then we extract `a` out of the result of `rma r` (which is an `m a`).
    4. Applying `f :: a -> ReaderT r m b)` to `a` yields a value of type `ReaderT r m b`.
    5. We now unpack the `r -> m b` structure out of `f a :: ReaderT r m b` using `runReaderT`.
    6. Finally, we apply this to `r`, to return an `m b` from the `do` block, as required.
