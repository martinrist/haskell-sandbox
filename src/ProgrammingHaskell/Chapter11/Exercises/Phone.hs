module ProgrammingHaskell.Chapter11.Exercises.Phone where

import Data.Char
import Data.List
import Data.Maybe

-- validButtons = "1234567890*#"
type Digit = Char

-- Valid presses: 1 and up
type Presses = Int

data Button =
    Button Digit
           [Char]
    deriving (Eq, Show)

data DaPhone =
    DaPhone [Button]
    deriving (Eq, Show)

standardPhone :: DaPhone
standardPhone =
    DaPhone
        [ Button '1' "1"
        , Button '2' "abc2"
        , Button '3' "def3"
        , Button '4' "ghi4"
        , Button '5' "jkl5"
        , Button '6' "mno6"
        , Button '7' "pqrs7"
        , Button '8' "tuv8"
        , Button '9' "wxyz9"
        , Button '*' "*"
        , Button '0' "+ 0"
        , Button '#' ".,#"
        ]

convo :: [String]
convo =
    [ "Wanna play 20 questions"
    , "Ya"
    , "U 1st haha"
    , "Lol ok. Have u ever tasted alcohol lol"
    , "Lol ya"
    , "Wow ur cool haha. Ur turn"
    , "Ok. Do u think I am pretty Lol"
    , "Lol ya"
    , "Haha thanks just making sure rofl ur turn"
    ]

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps p@(DaPhone bs) c
    | isUpper c = ('*', 1) : reverseTaps p (toLower c)
    | otherwise = mapMaybe (findChar c) bs

findChar :: Char -> Button -> Maybe (Digit, Presses)
findChar c (Button d cs) =
    case maybePresses of
        Nothing -> Nothing
        Just x -> Just (d, x + 1)
  where
    maybePresses = findIndex (\x -> x == c) cs

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead p s = foldr ((++) . (reverseTaps p)) [] s

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = foldr ((+) . snd) 0

frequencies :: (Ord a, Eq a) => [a] -> [(a, Int)]
frequencies = map (\xs -> (head xs, length xs)) . group . sort

mostPopularLetter :: String -> Char
mostPopularLetter = fst . (maximumBy compareSnd) . frequencies
  where
    compareSnd (_, ca) (_, cb) = compare ca cb
