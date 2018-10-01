module ProgrammingHaskell.Chapter08.Exercises.WordNumber where

import Data.List (intercalate)
import Test.Hspec

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord _ = ""

digits :: Int -> [Int]
digits n
    | d == 0 = [n]
    | otherwise = digits d ++ [m]
  where
    d = n `div` 10
    m = n `mod` 10

wordNumber :: Int -> String
wordNumber = intercalate "-" . map digitToWord . digits

main :: IO ()
main =
    hspec $
    describe "Returns expected results" $
    it "Returns expected results" $
    wordNumber 12324546 `shouldBe` "one-two-three-two-four-five-four-six"