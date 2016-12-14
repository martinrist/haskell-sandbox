module Cipher where

import Data.Char

caeasr :: Int -> [Char] -> [Char]
caesar n = map (shiftChar n)

shiftChar :: Int -> Char -> Char
shiftChar n c
  | c == ' '  = ' '
  | isUpper c = shift 'a' n
  | isLower c = shift 'A' n
  | otherwise = c
  where shift base n =  chr . (+ ord base) . (`mod` 26) . (+ n) . (subtract (ord base)) . ord
