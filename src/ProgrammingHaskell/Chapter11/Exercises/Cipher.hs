module ProgrammingHaskell.Chapter11.Exercises.Cipher where

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
    shiftFrom base n' =
        chr . (+ ord base) . (`mod` 26) . (+ n') . subtract (ord base) . ord

vignere :: String -> String -> String
vignere ks = polyAlphaSubst (concat . repeat $ ks)

polyAlphaSubst :: String -> String -> String
polyAlphaSubst _ [] = []
polyAlphaSubst [] ps = ps
polyAlphaSubst (k:ks) (p:ps)
    | p == ' ' = ' ' : polyAlphaSubst (k : ks) ps
    | otherwise = shift n p : polyAlphaSubst ks ps
  where
    n = ord (toUpper k) - ord 'A'
