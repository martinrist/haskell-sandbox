module ProgrammingHaskell.Chapter11.Exercises.Languages where

import Data.Char

capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord (x:xs) = toUpper x : xs