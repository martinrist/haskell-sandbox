{-# LANGUAGE InstanceSigs #-}

-------------------------------------
-- Chapter 26 - Monad Transformers --
-------------------------------------
main :: IO ()
main = undefined

-- This is how `MaybeT` is defined in `Control.Monad.Trans.Maybe`
newtype MaybeT m a =
    MaybeT { runMaybeT :: m (Maybe a) }

-- The `Functor` instance is similar to `Compose`...
instance (Functor m) => Functor (MaybeT m) where
    fmap f (MaybeT ma) =
        MaybeT $ (fmap . fmap) f ma


-- ... as is the `Applicative` instance
instance (Applicative m) => Applicative (MaybeT m) where
    pure x = MaybeT (pure (pure x))
    (MaybeT fab) <*> (MaybeT mma) =
        MaybeT $ (<*>) <$> fab <*> mma



instance (Monad m) => Monad (MaybeT m) where
    return = pure

    (>>=) :: MaybeT m a
          -> (a -> MaybeT m b)
          -> MaybeT m b

    (MaybeT ma) >>= f =
        MaybeT $ do
            v <- ma
            case v of
                 Nothing -> return Nothing
                 Just y  -> runMaybeT (f y)


-- 26.4 - ReaderT
-----------------

newtype ReaderT r m a =
    ReaderT { runReaderT :: r -> m a }


-- Functor for ReaderT
instance Functor m => Functor (ReaderT r m) where
    fmap f (ReaderT rma) =
        ReaderT $ (fmap . fmap) f rma

-- Applicative for ReaderT
instance Applicative m => Applicative (ReaderT r m) where
    pure a = ReaderT $ pure (pure a)

    (ReaderT fmab) <*> (ReaderT rma) =
        ReaderT $ (<*>) <$> fmab <*> rma

-- Monad for ReaderT
instance Monad m => Monad (ReaderT r m) where
    return = pure

    (>>=) :: ReaderT r m a
          -> (a -> ReaderT r m b)
          -> ReaderT r m b
    (ReaderT rma) >>= f =
        ReaderT $ \r -> do
            a <- rma r
            runReaderT (f a) r
