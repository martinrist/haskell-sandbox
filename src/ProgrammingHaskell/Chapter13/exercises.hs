module ProgrammingHaskell.Chapter13.Exercises where

-----------------------
-- Chapter Exercises --
-----------------------
import Control.Monad
import System.Exit (exitSuccess)
import Data.Char (toLower)

-- Exercises 2 & 3

palindrome :: IO ()
palindrome = forever $ do
    line1 <- getLine
    if isPalindrome line1 then putStrLn "It's a palindrome!" else do
        putStrLn "Nope!"
        exitSuccess

invalidChars :: [Char]
invalidChars = " '"

filterChars :: String -> String
filterChars = filter (`notElem` invalidChars)

prepare :: String -> String
prepare = map toLower . filterChars

isPalindrome :: String -> Bool
isPalindrome s = let s' = map toLower (filterChars s) in
                     s' == reverse s'


-- Exercise 4

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid = NameEmpty
                   | AgeTooLow
                   | PersonInvalidUnknown String
                   deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
    | name /= "" && age > 0 = Right $ Person name age
    | name == ""            = Left NameEmpty
    | age <= 0              = Left AgeTooLow
    | otherwise             = Left $ PersonInvalidUnknown $
                                "Name was: " ++ show name ++
                                " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
    putStr "Enter name: "
    name <- getLine
    putStr "Enter age: "
    age <- getLine
    let person = mkPerson name (read age) in
        outputPerson person


outputPerson :: Either PersonInvalid Person -> IO ()
outputPerson (Right p) = putStrLn $ "Yay! Successfully got a person: " ++ show p
outputPerson (Left i)  = putStrLn $ "Error: " ++ show i
