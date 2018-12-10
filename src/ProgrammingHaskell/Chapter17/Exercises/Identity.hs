module ProgrammingHaskell.Chapter17.Exercises.Identity where

import Test.QuickCheck
import Test.QuickCheck.Checkers

newtype Identity a = Identity a
    deriving (Eq, Ord, Show)

instance Semigroup a => Semigroup (Identity a) where
    Identity a <> Identity b = Identity $ a <> b

instance Monoid a => Monoid (Identity a) where
    mempty = Identity mempty

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
    (=-=) = eq

instance Functor Identity where
    fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
    pure = Identity
    Identity f <*> Identity a = Identity $ f a