module ProgrammingHaskell.Chapter11.Cipher where

import Data.Char

caesar :: Int -> [Char] -> [Char]
caesar n = map (shift n)

uncaesar :: Int -> [Char] -> [Char]
uncaesar = caesar . negate

shift :: Int -> Char -> Char
shift n c
  | isUpper c = shiftFrom 'A' n c
  | isLower c = shiftFrom 'a' n c
  | otherwise = c
  where shiftFrom base n =  chr . (+ ord base) . (`mod` 26) . (+ n) . (subtract (ord base)) . ord

vignere :: [Char] -> [Char] -> [Char]
vignere ks  = polyAlphaSubst (concat . repeat $ ks)

polyAlphaSubst :: [Char] -> [Char] -> [Char]
polyAlphaSubst _ [] = []
polyAlphaSubst [] ps = ps
polyAlphaSubst (k:ks) (p:ps)
    | p == ' '      = ' ' : polyAlphaSubst (k:ks) ps
    | otherwise     = (shift n p) : polyAlphaSubst ks ps
    where n = ord (toUpper k) - ord 'A'

