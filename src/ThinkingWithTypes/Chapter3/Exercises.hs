{-# LANGUAGE InstanceSigs #-}

module ThinkingWithTypes.Chapter3.Exercises where

import           Test.QuickCheck
import           Test.QuickCheck.Checkers

-- Exercise 3-i - Functor instances

newtype T1 a = T1 (Int -> a)

instance Functor T1 where
    fmap :: (a -> b) -> T1 a -> T1 b
    fmap f (T1 a) = T1 $ fmap f a

instance Arbitrary a => Arbitrary (T1 a) where
    arbitrary = T1 . const <$> arbitrary

instance Show a => Show (T1 a) where
    show (T1 a) = show (a 1)

instance (Eq a, EqProp a) => EqProp (T1 a) where
    (T1 a) =-= (T1 b) = a =-= b



newtype T5 a = T5 ((a -> Int) -> Int)

instance Functor T5 where
    fmap :: (a -> b) -> T5 a -> T5 b
    fmap f (T5 aii) = T5 $ \bi -> aii $ bi . f