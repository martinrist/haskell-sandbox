module ProgrammingHaskell.Chapter12.Exercises.StringProcessing where

import Data.Maybe

-- Question 1
notThe :: String -> Maybe String
notThe s =
    case s of
        "the" -> Nothing
        _ -> Just s

replaceThe :: String -> String
replaceThe = unwords . map (fromMaybe "a" . notThe) . words

-- Question 2
startsWithVowel :: String -> Bool
startsWithVowel "" = False
startsWithVowel (x:xs) = x `elem` "aeiou"

wordPairs :: [String] -> [(String, String)]
wordPairs ws = zip ws (tail ws)

countTheBeforeVowel :: String -> Int
countTheBeforeVowel = length . filter f . wordPairs . words
  where
    f (w1, w2) = w1 == "the" && startsWithVowel w2

-- Question 3
countVowels :: String -> Integer
countVowels = fromIntegral . length . filter (`elem` "aeiou")