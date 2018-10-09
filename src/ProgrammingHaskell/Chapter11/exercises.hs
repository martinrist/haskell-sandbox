{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Exercises where

import Data.Char

-- Chapter Exercises: As-patterns
---------------------------------
-- Question 1
isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf xall@(x:xs) (y:ys)
    | x == y = isSubsequenceOf xs ys
    | otherwise = isSubsequenceOf xall ys

-- Question 2
capitaliseWords :: String -> [(String, String)]
capitaliseWords ws = [(w, capitaliseWord w) | w <- words ws]

-- Chapter Exercises: Language Exercises
----------------------------------------
-- Question 1
capitaliseWord :: String -> String
capitaliseWord "" = ""
capitaliseWord (x:xs) = toUpper x : xs
