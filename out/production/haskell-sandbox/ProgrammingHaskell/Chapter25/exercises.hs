{-# LANGUAGE InstanceSigs #-}

import           Data.Monoid

----------------------------
-- Chapter 25 - Exercises --
----------------------------

main :: IO ()
main = undefined


---------------------------------------
-- 25.6 Exercises: Compose Instances --
---------------------------------------

newtype Compose f g a =
    Compose { getCompose :: f (g a) }
    deriving (Eq, Show)



-- Foldable
-----------

instance (Foldable f, Foldable g) =>
          Foldable (Compose f g) where
    foldMap :: Monoid m => (a -> m) -> Compose f g a -> m
    foldMap fam (Compose fga) = (foldMap . foldMap) fam fga



-- Traversable
--------------
instance (Functor f, Functor g) =>
          Functor (Compose f g) where
    fmap f (Compose fga) =
        Compose $ (fmap . fmap) f fga

instance (Traversable f, Traversable g) =>
          Traversable (Compose f g) where
    traverse :: Applicative p => (a -> p b) -> Compose f g a -> p (Compose f g b)
    traverse fapb (Compose fga) = Compose <$> (traverse . traverse) fapb fga


-- Bifunctor instances
----------------------

class Bifunctor p where
    {-# MINIMAL bimap | first, second #-}

    bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
    bimap f g = first f . second g

    first :: (a -> b)  -> p a c -> p b c
    first f = bimap f id

    second :: (b -> c) -> p a b -> p a c
    second = bimap id


-- 1 - Deux
data Deux a b = Deux a b
    deriving (Eq, Show)

instance Bifunctor Deux where
    bimap f g (Deux a b) = Deux (f a) (g b)


-- 2 - Const
data Const a b = Const a
    deriving (Eq, Show)

instance Bifunctor Const where
    bimap f g (Const a) = Const $ f a


-- 3 - Drei
data Drei a b c = Drei a b c
    deriving (Eq, Show)

instance Bifunctor (Drei a) where
    bimap f g (Drei a b c) = Drei a (f b) (g c)


-- 4 - SuperDrei
data SuperDrei a b c = SuperDrei a b
    deriving (Eq, Show)

instance Bifunctor (SuperDrei a) where
    bimap f g (SuperDrei a b) = SuperDrei a (f b)


-- 5 - SemiDrei
data SemiDrei a b c = SemiDrei a
    deriving (Eq, Show)

instance Bifunctor (SemiDrei a) where
    bimap f g (SemiDrei a) = SemiDrei a


-- 6 - Quadriceps
data Quadriceps a b c d = Quadzz a b c d
    deriving (Eq, Show)

instance Bifunctor (Quadriceps a b) where
    bimap f g (Quadzz a b c d) = Quadzz a b (f c) (g d)


-- 7 - Either
instance Bifunctor Either where
    bimap f g (Left a)  = Left (f a)
    bimap f g (Right b) = Right (g b)
