-----------------------
-- Chapter Exercises --
-----------------------
import Data.Maybe

-- String processing
--------------------

-- Question 1

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe s     = Just s

-- Recursive implementation - looks a little clunky
replaceThe :: String -> String
replaceThe = unwords . replaceThe' . words
    where replaceThe' :: [String] -> [String]
          replaceThe' (w:[]) = case (notThe w) of
                                  Nothing -> "a" : []
                                  Just s  -> s : []
          replaceThe' (w:ws) = case (notThe w) of
                                  Nothing -> "a" : replaceThe' ws
                                  Just s  -> s : replaceThe' ws


-- Much more elegant version using `fromMaybe`
replaceThe' :: String -> String
replaceThe' = unwords . map ( fromMaybe "a" . notThe) . words


-- Question 2
isVowel :: Char -> Bool
isVowel = (flip elem) "aeiou"

startsWithVowel :: String -> Bool
startsWithVowel "" = False
startsWithVowel (x:xs) = isVowel x

wordPairs :: [String] -> [(String, String)]
wordPairs ws = zip ws (tail ws)

countTheBeforeVowel :: String -> Int
countTheBeforeVowel = 
          length . filter f . wordPairs . words
            where f :: (String, String) -> Bool
                  f (w1, w2) = w1 == "the" && startsWithVowel w2


-- Question 3

countVowels :: String -> Int
countVowels =  foldr ((+).length) 0 . map (\w -> (filter isVowel w)) . words


-- Validate the word
--------------------

newtype Word' =
    Word' String
    deriving (Eq, Show)

vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord s = let numVowels = length (filter isVowel s)
               numConsonants = length s - numVowels in
        if (countVowels s) > (length s - countVowels s) then Nothing else Just (Word' s)
