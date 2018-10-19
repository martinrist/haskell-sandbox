module ProgrammingHaskell.Chapter13.Exercises.Palindrome where

import           Control.Monad
import           System.Exit                    ( exitSuccess )
import           Data.Char

-- Question 2
palindrome :: IO ()
palindrome = forever $ do
    line1 <- getLine
    if line1 == reverse line1
        then putStrLn "It's a palindrome"
        else do
            putStrLn "Nope!"
            exitSuccess

-- Question 3
invalidChars :: [Char]
invalidChars = " '"

filterChars :: String -> String
filterChars = filter (`notElem` invalidChars)

prepare :: String -> String
prepare = map toLower . filterChars

isPalindrome :: String -> Bool
isPalindrome s = let s' = map toLower (filterChars s) in s' == reverse s'