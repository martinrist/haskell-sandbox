module ProgrammingHaskell.Chapter16.Exercises.Functor where

import ProgrammingHaskell.Chapter15.Exercises.Semigroup
import Test.QuickCheck
import Text.Show.Functions
import Control.Applicative

-- Question 1 - Identity

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)


-- Question 2 - Pair

data Pair a = Pair a a deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Pair a) where
    arbitrary = Pair <$> arbitrary <*> arbitrary

instance Functor Pair where
    fmap f (Pair x y) = Pair (f x) (f y)


-- Question 3 - Two

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)


-- Question 4 - Three

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)


-- Question 5 - Three'

data Three' a b = Three' a b b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = liftA3 Three' arbitrary arbitrary arbitrary

instance Functor (Three' a) where
    fmap f (Three' x y z) = Three' x (f y) (f z)


-- Question 6 - Four

instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)


-- Question 7 - Four'

data Four' a b = Four' a a a b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
    arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Functor (Four' a) where
    fmap f (Four' a b c d) = Four' a b c (f d)
