{-# LANGUAGE InstanceSigs #-}

import           Control.Monad
import           Control.Monad.Trans.Class


----------------------------
-- Chapter 26 - Exercises --
----------------------------

main :: IO ()
main = undefined



-- 26.3 - EitherT
-----------------

newtype EitherT e m a =
    EitherT { runEitherT :: m (Either e a) }

-- 1. Functor instance for EitherT
instance Functor m => Functor (EitherT e m) where
    fmap f (EitherT meea) =
        EitherT $ (fmap . fmap) f meea

-- 2. Applicatibe instance for EitherT
instance Applicative m => Applicative (EitherT e m) where
    pure x = EitherT (pure (pure x))
    (EitherT efab) <*> (EitherT ea) =
        EitherT $ (<*>) <$> efab <*> ea

-- 3. Monad instance for EitherT
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

-- 4. swapEitherT
swapEither :: Either e a -> Either a e
swapEither ea = case ea of
                     Left e  -> Right e
                     Right a -> Left a

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT ema) =
    EitherT $ swapEither <$> ema


eitherT :: Monad m => (a -> m c)
                   -> (b -> m c)
                   -> EitherT a m b
                   -> m c
eitherT famc fbmc (EitherT amb) = do
    v <- amb
    case v of
         Left a  -> famc a
         Right b -> fbmc b



-- 26.5 - StateT
----------------

newtype StateT s m a =
    StateT { runStateT :: s -> m (a, s) }


-- 1. Functor instance
instance (Functor m) => Functor (StateT s m) where
    fmap f (StateT sma) =
        StateT $ \s ->
            fmap (\(a, s') -> (f a, s')) (sma s)


-- 2. Applicative instance
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



-- 3. Monad instance
instance (Monad m) => Monad (StateT s m) where
    return = pure

    (>>=) :: StateT s m a
          -> (a -> StateT s m b)
          -> StateT s m b
    (StateT sma) >>= f =
        StateT $ \s -> do
            (a, s')  <- sma s
            runStateT (f a) s'


-- Exercises : Lift More
------------------------

instance MonadTrans (EitherT e) where
    lift :: Monad m => m a -> EitherT e m a
    lift = EitherT . fmap Right


