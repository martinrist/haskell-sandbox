module ProgrammingHaskell.Chapter13.Exercises.Person where

import           Safe

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid = NameEmpty
                   | AgeTooLow
                   | PersonInvalidUnknown String
                   deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
    | name /= "" && age > 0
    = Right $ Person name age
    | name == ""
    = Left NameEmpty
    | age <= 0
    = Left AgeTooLow
    | otherwise
    = Left
        $  PersonInvalidUnknown
        $  "Name was: "
        ++ show name
        ++ "Age was: "
        ++ show age

gimmePerson :: IO ()
gimmePerson = do
    putStr "Please enter name: "
    n <- getLine
    putStr "Please enter age: "
    a <- getLine
    case readMay a of
        Nothing -> putStrLn "An error occurred: Unable to read age"
        Just a' -> case mkPerson n a' of
            Left  e -> putStrLn $ "An error occurred: " ++ show e
            Right p -> putStrLn $ "Yay! Successfully got a person: " ++ show p