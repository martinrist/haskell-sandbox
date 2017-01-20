-----------------------
-- Chapter Exercises --
-----------------------

module Exercises where

import Test.QuickCheck
import Data.List (sort)


-- Exercise 1 - half

half :: Fractional a => a -> a
half x = x / 2

halfIdentity :: Fractional a => a -> a
halfIdentity = (*2).half

prop_checkHalfIdentity :: (Eq a, Fractional a) => a -> Bool
prop_checkHalfIdentity x = x == halfIdentity x

exercise1 :: IO ()
exercise1 = quickCheck (prop_checkHalfIdentity :: Float -> Bool)


-- Exercise 2 - list sorting

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


-- Exercise 3 - associativity and commutativity of +

plusAssociative x y z =
    x + (y + z) == (x + y) + z

plusCommutative x y = x + y == y + x

prop_associative :: (Eq a, Num a) => a -> a -> a -> Bool
prop_associative = plusAssociative

prop_commutative :: (Eq a, Num a) => a -> a -> Bool
prop_commutative = plusCommutative

exercise3 :: IO ()
exercise3 = do
    quickCheck (prop_associative :: Int -> Int -> Int -> Bool)
    quickCheck (prop_commutative :: Int -> Int -> Bool)


-- Exercise 4 - associativity and commutativity of *

multiplyAssociative x y z =
    x * (y * z) == (x * y) * z

multiplyCommutative x y =
    x * y == y * x

prop_multiply_associative :: (Eq a, Num a) => a -> a -> a -> Bool
prop_multiply_associative = multiplyAssociative

prop_multiply_commutative :: (Eq a, Num a) => a -> a -> Bool
prop_multiply_commutative = multiplyCommutative

exercise4 :: IO ()
exercise4 = do
    quickCheck (prop_multiply_associative :: Int -> Int -> Int -> Bool)
    quickCheck (prop_multiply_commutative :: Int -> Int -> Bool)


-- Exercise 5 - testing quot / rem and div / mod

prop_quotRem :: (Eq a, Integral a) => a -> a -> Bool
prop_quotRem _ 0 = True
prop_quotRem x y = quot x y * y + rem x y == x

prop_divMod :: (Eq a, Integral a) => a -> a -> Bool
prop_divMod _ 0 = True
prop_divMod x y = div x y * y + mod x y == x

exercise5 :: IO ()
exercise5 = do
    quickCheck (prop_quotRem :: Int -> Int -> Bool)
    quickCheck (prop_divMod :: Int -> Int -> Bool)


-- Exercise 6 - non-associativity and non-commutativity of ^

prop_power_associative :: (Eq a, Integral a) => a -> a -> a -> Bool
prop_power_associative x y z =
    (x ^ y) ^ z == x ^ (y ^ z)

prop_power_commutative :: (Eq a, Integral a) => a -> a -> Bool
prop_power_commutative x y =
    x ^ y == y ^ x

exercise6 :: IO ()
exercise6 = do
    quickCheck (prop_power_associative :: Int -> Int -> Int -> Bool)
    quickCheck (prop_power_commutative :: Int -> Int -> Bool)


-- Exercise 7 - Reversing a list

prop_reverseTwice :: Eq a => [a] -> Bool
prop_reverseTwice xs = (reverse . reverse) xs == xs

exercise7 :: IO ()
exercise7 = quickCheck (prop_reverseTwice :: [Char] -> Bool)


-- Exercise 8 - definition of $
-- TODO: Investigate use of CoArbitrary for thisucc
