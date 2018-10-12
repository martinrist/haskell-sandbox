module ProgrammingHaskell.Chapter12.Exercises.ValidateWord where

import Data.Char

isVowel :: Char -> Bool
isVowel = flip elem "aeiou"

isConsonant :: Char -> Bool
isConsonant c = isAlpha c && not (isVowel c)

newtype Word' =
    Word' String
    deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord s =
    if numVowels s > numConsonants s
        then Nothing
        else Just $ Word' s
  where
    numVowels = length . filter isVowel
    numConsonants = length . filter isConsonant