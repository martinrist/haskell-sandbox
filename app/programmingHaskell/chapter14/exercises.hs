{-# OPTIONS_GHC -Wall #-}
-----------------------
-- Chapter Exercises --
-----------------------

module Exercises where

import Test.QuickCheck
import Data.List (sort)


-- Exercise 1

half :: Fractional a => a -> a
half x = x / 2

halfIdentity :: Fractional a => a -> a
halfIdentity = (*2).half

prop_checkHalfIdentity :: (Eq a, Fractional a) => a -> Bool
prop_checkHalfIdentity x = x == halfIdentity x

exercise1 :: IO ()
exercise1 = quickCheck (prop_checkHalfIdentity :: Float -> Bool)


-- Exercise 2

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
    snd $ foldr go (Nothing, True) xs
    where go _ status@(_, False) = status
          go y (Nothing, t) = (Just y, t)
          go y (Just x, t) = (Just y, x >= y)

prop_sortOrdersList :: Ord a => [a] -> Bool
prop_sortOrdersList = listOrdered . sort

exercise2 :: IO ()
exercise2 = quickCheck (prop_sortOrdersList :: [String] -> Bool)


-- Exercise 3

plusAssociative x y z =
    x + (y + z) == (x + y) + z

plusCommutative x y = x + y == y + x

prop_associative :: (Eq a, Num a) => a -> a -> a -> Bool
prop_associative = plusAssociative

prop_commutative :: (Eq a, Num a) => a -> a -> Bool
prop_commutative = plusCommutative

exercise3 :: IO ()
exercise3 = do
    quickCheck prop_associative
    quickCheck prop_commutative