module ProgrammingHaskell.Chapter09.Exercises.Cipher where

import Data.Char

caesar :: Int -> String -> String
caesar n = map (shift n)

uncaesar :: Int -> String -> String
uncaesar = caesar . negate

shift :: Int -> Char -> Char
shift n c
    | isUpper c = shiftFrom 'A' n c
    | isLower c = shiftFrom 'a' n c
    | otherwise = c
  where
    shiftFrom base n' = chr . (+ ord base) . (`mod` 26) . (+ n') . subtract (ord base) . ord
