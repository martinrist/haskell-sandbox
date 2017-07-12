{-# LANGUAGE InstanceSigs #-}

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
