module RealWorld.Chapter03.Exercises where

import Data.List (sortBy, sortOn)

-- 1. Function that computes the number of elements in a list
-- 2. Type signature for exercise 1

elements :: [a] -> Int
elements [] = 0
elements (_:xs) = 1 + elements xs


-- 3. Function that computes the mean of a list

mean :: [Int] -> Double
mean xs = fromIntegral (sum xs) / fromIntegral (length xs)


-- 4. Turns a list into a palindrome

toPalindrome :: [a] -> [a]
toPalindrome xs = xs ++ reverse xs


-- 5. Test whether a list is a palindrome

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs


-- 6. Sorts a list of lists based on the length of each sublist

sortBySublistLength :: [[a]] -> [[a]]
sortBySublistLength = sortBy lengthComparator
    where lengthComparator xs ys = compare (length xs) (length ys)


-- 7. Joins a list of lists togetehr using a seprator value

intersperse :: a -> [[a]] -> [a]
intersperse _ [] = []
intersperse _ [l] = l
intersperse s (l:ls) = l ++ [s] ++ intersperse s ls


-- 8. Write function to determine height of a binary tree

data Tree a
    = Node a (Tree a) (Tree a)
    | Empty
    deriving (Show)

height :: Tree a -> Int
height Empty = 0
height (Node _ t1 t2) = 1 + max (height t1) (height t2)