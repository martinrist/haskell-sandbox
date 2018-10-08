module ProgrammingHaskell.Chapter10.Exercises.StopsAndVowels where

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

stopVowelStop :: String -> String -> [(Char, Char, Char)]
stopVowelStop ss vs = [(s, v, t) | s <- ss, v <- vs, t <- ss]