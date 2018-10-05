module ProgrammingHaskell.Chapter06.Exercises.MatchTheTypes where

import Data.List (sort)

-- Question 1
i1 :: Num a => a
-- i1 :: a
i1 = 1

-- Question 2
f2 :: Float
--f2 :: Num a => a          -- 1.0 is `Fractional` so this doesn't work
f2 = 1.0

-- Question 3
-- f3 :: Float
f3 :: Fractional a => a
f3 = 1.0

-- Question 4
-- f4 :: Float
f4 :: RealFrac a => a -- This works because `Fractional a => RealFrac a`
f4 = 1.0

-- Question 5
-- freud :: a -> a
freud :: Ord a => a -> a -- This works but is superfluous
freud x = x

-- Question 6
-- freud' :: a -> a
freud' :: Int -> Int -- This works but is concretised
freud' x = x

-- Question 7
myX :: Int
myX = 1 :: Int

sigmund :: Int -> Int
-- sigmund :: a -> a -- This doesn't work because `myX :: Int`
sigmund _ = myX

-- Question 8
myX' :: Int
myX' = 1 :: Int

sigmund' :: Int -> Int
-- sigmund' :: Num a => a -> a -- This doesn't work because `myX :: Int`
sigmund' _ = myX' -- Question 9

-- Question 9
-- jung :: Ord a => [a] -> a
jung :: [Int] -> Int -- This works, because `Int` has an instance of `Ord`
jung xs = head (sort xs)

-- Question 10
--young :: [Char] -> Char
young :: Ord a => [a] -> a -- This will work as `Ord` has `sort`
young xs = head (sort xs)

-- Question 11
mySort :: [Char] -> [Char]
mySort = sort

signifier :: [Char] -> Char
-- signifier :: Ord a => [a] -> a -- This won't work because `mySort` is already concretised
signifier xs = head (mySort xs)