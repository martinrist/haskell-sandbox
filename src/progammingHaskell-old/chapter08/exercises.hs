------------------------
-- Exercises - Chapter 8
------------------------

import Data.List (intersperse)

--------------------
-- Chapter exercises
--------------------

-- Recursion
------------

-- Question 2

sum' :: (Eq a, Num a, Ord a) => a -> a
sum' x
    | x < 0     = 0
    | x == 1    = 1
    | otherwise = x + sum' ( x - 1 )


-- Question 3
multiply :: Integral a => a -> a -> a
multiply x y = go 0 x y
    where go acc x y
            | y == 0    = acc
            | otherwise = go (acc + x) x (y - 1)


-- McCarthy 91 function
-----------------------

mc91 :: (Ord a, Num a) => a -> a
mc91 n
     | n > 100    = n - 10
     | otherwise  = mc91 . mc91 $ (n + 11)


-- Numbers into words
---------------------

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord _ = ""

digits :: Int -> [Int]
digits n
       | d == 0    = [n]
       | otherwise = digits d ++ m:[]
       where d = n `div` 10
             m = n `mod` 10

wordNumber :: Int -> String
wordNumber = concat . intersperse "-" . map digitToWord . digits