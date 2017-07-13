{-# LANGUAGE InstanceSigs #-}

import           Control.Monad (join)

--------------------------------------------
-- Chapter 25 - Compsing Types - Examples --
--------------------------------------------
main :: IO ()
main = undefined


-- 25.2 - Common functions as types
-----------------------------------

-- The Identity newtype corresponds to the `id` function
newtype Identity a =
    Identity { runIdentity :: a }
    deriving (Eq, Show)

-- The Compose newtype corresponds to the `(.)` function
newtype Compose f g a =
    Compose { getCompose :: f (g a) }
    deriving (Eq, Show)



-- 25.3 - Composing Functors
----------------------------

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance (Functor f, Functor g) =>
    Functor (Compose f g) where
    fmap f (Compose fga) = Compose $ (fmap . fmap) f fga


-- 25.4 - Twinplicative
-----------------------

instance Applicative Identity where
    pure = Identity
    (Identity f) <*> (Identity a) = Identity $ f a

instance (Applicative f, Applicative g) =>
         Applicative (Compose f g) where
    pure :: a -> Compose f g a
    pure a = Compose $ pure (pure a)

    (<*>) :: Compose f g (a -> b)
          -> Compose f g a
          -> Compose f g b
    (Compose fgab) <*> (Compose fga) =
        Compose $ (<*>) <$> fgab <*> fga



-- 25.8 - IdentityT
-------------------


newtype IdentityT f a =
    IdentityT { runIdentityT :: f a }
    deriving (Eq, Show)

instance Functor m => Functor (IdentityT m) where
    fmap f (IdentityT fa) = IdentityT (fmap f fa)

instance Applicative m => Applicative (IdentityT m) where
    pure x = IdentityT (pure x)
    (IdentityT fab) <*> (IdentityT fa) =
        IdentityT (fab <*> fa)

instance Monad m => Monad (IdentityT m) where
    return = pure

    (>>=) :: IdentityT m a
          -> (a -> IdentityT m b)
          -> IdentityT m b
    (IdentityT ma) >>= f =
        IdentityT $ ma >>= runIdentityT . f

