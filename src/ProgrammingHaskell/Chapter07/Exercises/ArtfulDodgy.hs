module ProgrammingHaskell.Chapter07.Exercises.ArtfulDodgy where

import Test.Hspec

dodgy :: Num a => a -> a -> a
dodgy x y = x + y * 10

oneIsOne :: Num a => a -> a
oneIsOne = dodgy 1

oneIsTwo :: Num a => a -> a
oneIsTwo = flip dodgy 2

main :: IO ()
main =
    hspec $ do
        describe "Question 1" $ it "dodgy 1 0 == 1" $ dodgy 1 0 `shouldBe` 1
        describe "Question 2" $ it "dodgy 1 1 = 11" $ dodgy 1 1 `shouldBe` 11
        describe "Question 3" $ it "dodgy 2 2 = 22" $ dodgy 2 2 `shouldBe` 22
        describe "Question 4" $ it "dodgy 1 2 = 21" $ dodgy 1 2 `shouldBe` 21
        describe "Question 5" $ it "dodgy 2 1 = 12" $ dodgy 2 1 `shouldBe` 12
        describe "Question 6" $ it "oneIsOne 1 = 11" $ oneIsOne 1 `shouldBe` 11
        describe "Question 7" $ it "oneIsOne 2 = 21" $ oneIsOne 2 `shouldBe` 21
        describe "Question 8" $ it "oneIsTwo 1 = 21" $ oneIsTwo 1 `shouldBe` 21
        describe "Question 9" $ it "oneIsTwo 2 = 22" $ oneIsTwo 2 `shouldBe` 22
        describe "Question 10" $ it "oneIsOne 3 = 31" $ oneIsOne 3 `shouldBe` 31
        describe "Question 11" $ it "oneIsTwo 3 = 23" $ oneIsTwo 3 `shouldBe` 23