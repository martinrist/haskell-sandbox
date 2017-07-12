{-# LANGUAGE InstanceSigs #-}

import Data.Monoid

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
