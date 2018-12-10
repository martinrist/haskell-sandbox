module ProgrammingHaskell.Chapter17.Exercises.Constant where

import Test.QuickCheck
import Test.QuickCheck.Checkers

newtype Constant a b =
    Constant { getConstant :: a }
    deriving (Eq, Ord, Show)

instance Semigroup a => Semigroup (Constant a b) where
    Constant a <> Constant a' = Constant $ a <> a'

instance Monoid a => Monoid (Constant a b) where
    mempty = Constant mempty

instance Arbitrary a => Arbitrary (Constant a b) where
    arbitrary = Constant <$> arbitrary

instance Eq a => EqProp (Constant a b) where
    (=-=) = eq

instance Functor (Constant a) where
    fmap _ (Constant a) = Constant $ a

instance Monoid a => Applicative (Constant a) where
    pure x = Constant mempty
    (Constant a) <*> (Constant a') = Constant $ (a <> a')