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


## 25.6 - `StateT`

- Similar to `ReaderT`, `StateT` is `State` but with extra monadic structure wrapped around the `(a, s`):

    ```haskell
    newtype StateT s m a -
        StateT { runStateT :: s -> m (a, s) }
    ```

- Unlike with other transformers, we can't just fall back to the standard patterns (e.g. `fmap.fmap` for `Functor`, `FooT (<$>) fmab <*> ma` for `Applicative`).  Instead we need to write our own versions that carry along the state correctly:

    ```haskell
    instance (Functor m) => Functor (StateT s m) where
        fmap f (StateT sma) =
            StateT $ \s ->
                fmap (\(a, s') -> (f a, s')) (sma s)

    instance (Monad m) => Applicative (StateT s m) where
        pure a = StateT $ \s -> pure (a, s)

        (<*>) :: StateT s m (a -> b)
              -> StateT s m a
              -> StateT s m b
        (StateT smfab) <*> (StateT sma) =
            StateT $ \s -> do
               (fab, s') <- smfab s
               (a, s'')  <- sma s'
               return (fab a, s'')
    ```

- Note how similar the `Monad` instance looks to the one for `ReaderT`:

    ```haskell
    instance (Monad m) => Monad (StateT s m) where
        return = pure

        (>>=) :: StateT s m a
              -> (a -> StateT s m b)
              -> StateT s m b
        (StateT sma) >>= f =
            StateT $ \s -> do
                (a, s')  <- sma s
                runStateT (f a) $ s'
    ```


## 26.6 - Types we don't want to use

- `ListT` and `WriterT` are examples of types that aren't necessarily performant or don't make sense.

- `Writer` / `WriterT` (the opposite of `Reader` / `ReaderT`) are often either too lazy or too strict:
    - They can often end up using too much memory as a result.

- The most obvious way to implement `ListT` isn't generally recommended:
    - Most initial attempts won't pass the associativity law.
    - Correct implementations are not very fast.
    - Streaming libraries like [`pipes`](http://hackage.haskell.org/package/pipes) or [`conduit`](http://hackage.haskell.org/package/conduit) do it better for most use cases.



## 26.7 - Recovering an ordinary type from a transformer

- If we have a transformer variety of a type and we want to get back the plain type, we just need some `m` structure that does nothing:
    - The `Identity` monad is the obvious candidate.

- So we can retrieve the non-transfomer types thus:

    ```haskell
    type Maybe a    = MaybeT Identity a
    type Either e a = EitherT e Identity a
    type Reader r a = ReaderT r Identity a
    type State s a  = StateT s Identity a
    ```


## 26.8 - Lexically inner is structurally outer

- Look again at the structure of some common monad transformers:

    ```haskell
    newtype MaybeT m a =
        MaybeT { runMaybeT :: m (Maybe a) }

    newtype ExceptT e m a =
        ExceptT { runExceptT :: m (Either e a)) }

    newtype ReaderT r m a =
        ReaderT { runReaderT :: r -> m a }
    ```

- Note how the additional structure `m` is always wrapped _around_ our value - e.g. the `Maybe` in `MaybeT` is inside the `m`:
    - So, a series of monad transformers will start with the structurally-innermost type and work outwards.

- Consider this example:

    ```haskell
    -- Use the libray versions, not our own ones
    import Control.Monad.Trans.Except
    import Control.Monad.Trans.Maybe

    embedded :: MaybeT (ExceptT String []) Int
    embedded = return 1

    maybeUnwrap :: ExceptT String [] (Maybe Int)
    maybeUnwrap = runMaybeT embedded

    eitherUnwrap :: [Either String (Maybe Int)]
    eitherUnwrap = runExceptT maybeUnwrap

    -- Note how the first transformer (MaybeT) is the innermost
    -- and the last transformer ([]) is the outermost
    > embedded
    MaybeT (ExceptT [Right (Just 1)])

    > maybeUnwrap
    ExceptT [Right (Just 1)]

    > eitherUnwrap
    [Right (Just 1)]
    ``


## 26.9 - `MonadTrans`

- We often want to _lift_ a function into a larger context, as we do for `Functor`, `Applicative` and `Monad`:

    ```haskell
    fmap  :: Functor f     => (a -> b) -> f a -> f b

    liftA :: Applicative f => (a -> b) -> f a -> f b

    liftM :: Monad m       => (a -> b) -> m a -> m b
    ```

- In some cases, we want to talk about more or different structure than offered by the above:
    - e.g we may want something that does as much lifting as necessary to reach the structurally outermost position in a stack of monad transformers.

- `MonadTrans` is a typeclass with one method `lift`:

    ```haskell
    > import Control.Monad.Trans.Class
    > :i MonadTrans
    class MonadTrans t where
        lift :: Monad m => m a -> t m a
    ```

- Here, the `t` is a monad transformer type that has an instance of `MonadTrans` defined, and `m` is any generic Monad.

- As motivation for `MonadTrans`, consider the web framework [`scotty`](http://hackage.haskell.org/package/scotty).

- Here are some of the types defined in {{scotty}}:

    ```haskell
    newType ScottyT e m a =
        ScottyT { runS :: State (ScottyState e m) a }
        deriving (Functor, Applicative, Monad)

    type ScottyM = ScottyT Text IO

    newType ActionT e m a =
        ActionT { runAM :: ExceptT (ActionError e)
                                   (ReaderT ActionEnv
                                   (StateT ScottyResponse m)) a }
        deriving (Functor, Applicative)

    -- Note that the `M` variants are just type synonyms for
    -- the transformers, with the inner types already set
    type ActionM = ActionT Text IO
    ```

- Here's an example Scotty app:

    ```haskell
    main = scotty 3000 $
        get "/:word" $ do
            beam <- param "word"
            html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
    ```

- Let's interrogate some of the types:

    ```haskell
    > :t scotty
    scotty :: Port -> ScottyM () -> IO ()
    -- So, the result of `get ...` in the example app is `ScottyM ()`

    > :t get
    get :: RoutePattern -> ActionM () -> ScottyM ()
    -- So, the result of the `do` block should be `ActionM ()`

    > :t html
    html :: Text -> ActionM ()
    ```

- Say we want to output the value of `beam` in the `do` block:

    ```haskell
    main = scotty 3000 $
        get "/:word" $ do
            beam <- param "word"
            putStrLn "hello"
            html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
    ```

- Since `putStrLn beam :: IO ()` we'll get a type error since it needs to be `ActionM ()`.

- To turn the `IO ()` into `ActionM ()`, we apply `lift`:

    ```haskell
    > :t lift
    lift :: (Monad m, MonadTrans t) => m a -> t m a

    -- m ~ IO
    > :t lift (putStrLn "output")
    lift (putStrLn "output") :: MonadTrans t => t IO ()

    -- Here, we're heading towards `ActionM = ActionT Text IO`
    -- and the `MonadTrans` instance is defined on `ActionT Text`
    -- so `t ~ ActionT Text`:

    ... lift (putStrLn "output") :: ActionT Text IO ()
    ...                          :: ActionM ()
    ```

- We can work out exactly what `lift` does in the `MonadTrans` instance for `ActionT` by looking at the [source code](https://github.com/scotty-web/scotty/blob/0.10.2/Web/Scotty/Internal/Types.hs#L157):

    ```haskell
    instance MonadTrans (ActionT e) where
        lift = ActionT . lift . lift . lift
    ```

- This is reusing and composing the `lift` implementations for the three additional monad transformers that comprise `ActionT`, namely `ExceptT`, `ReaderT` and `StateT`:

    ```haskell
    instance MonadTrans (ExceptT e) where
        lift = ExceptT . liftM Right

    instance MonadTrans (ReaderT r) where
        lift = ReaderT . const

    instance MonadTrans (StateT s) where
        lift m = StateT $ \s -> do
            a <- m
            return (a, s)
    ```

- Some other `MonadTrans` instances:

    ```haskell
    instance MonadTrans IdentityT where
        lift = IdentityT

    -- Roughly speaking , this takes an `m a` and lifts it into
    -- a `MaybeT` context:
    instance MonadTrans MaybeT where
        lift = MaybeT . liftM Just
    ```


## 26.10 - `MonadIO` and `liftIO`

- `MonadIO` is similar to `MonadTrans` but different in that, rather than lifting through one 'layer' of a monad transformer stack, it keeps lifting an IO action until it's lifted over all structure embedded in the outermost IO type.

    ```haskell
    > import Control.Monad.IO.Class
    > :t liftIO
    liftIO :: MonadIO m => IO a -> m a
    ```

- Some example instances:

    ```haskell
    instance (MonadIO m) => MonadIO (IdentityT m) where
        liftIO = IdentityT . liftIO

    instance (MonadIO m) => MonadIO (MaybeT m) where
        liftIO = lift . liftIO

    instance (MonadIO m) => MonadIO (EitherT e m) where
        liftIO = lift . liftIO
    ```
