module ProgrammingHaskell.Chapter11.Exercises.AsPatterns where

import Data.Char

-- Question 1
isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf xall@(x:xs) (y:ys)
    | x == y = isSubseqOf xs ys
    | otherwise = isSubseqOf xall ys

-- Question 2
capitaliseWords :: String -> [(String, String)]
capitaliseWords = map toCapitalised . words
  where
    toCapitalised [] = ([], [])
    toCapitalised xall@(x:xs) = (xall, toUpper x : xs)