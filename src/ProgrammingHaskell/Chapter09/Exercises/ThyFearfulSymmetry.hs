{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module ProgrammingHaskell.Chapter09.Exercises.ThyFearfulSymmetry where

import Data.Char

-- Question 1
myWords :: String -> [String]
myWords [] = []
myWords s =
    let first = takeWhile (not . isSpace) s
        rest = dropWhile isSpace . dropWhile (not . isSpace) $ s
     in first : myWords rest

-- Question 2
myLines :: String -> [String]
myLines [] = []
myLines s =
    let first = takeWhile (not . isNewline) s
        rest = dropWhile isNewline . dropWhile (not . isNewline) $ s
     in first : myLines rest
  where
    isNewline c = c == '\n'
