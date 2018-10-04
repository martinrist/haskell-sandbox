{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module ProgrammingHaskell.Chapter09.Exercises.ThyFearfulSymmetrySpec where

import Data.List
import ProgrammingHaskell.Chapter09.Exercises.ThyFearfulSymmetry
import Test.Hspec

testMyWords :: Spec
testMyWords =
    context "myWords" $ do
        it "Works for supplied example" $
            myWords "sheryl wants fun" `shouldBe` ["sheryl", "wants", "fun"]
        it "Returns empty list for empty imput" $ myWords "" `shouldBe` []
        it "Returns single string for single word" $
            myWords "foo" `shouldBe` ["foo"]
        it "Returns two strings for two words separated by single space" $
            myWords "foo bar" `shouldBe` ["foo", "bar"]
        it "Returns two strings for two words separated by multiple spaces" $
            myWords "foo    bar" `shouldBe` ["foo", "bar"]
        it "Treats tabs and newlines like whitespace" $
            myWords "foo bar\tbaz\nblort\rquux" `shouldBe`
            ["foo", "bar", "baz", "blort", "quux"]

firstSen = "Tyger Tyger, burning bright\n"

secondSen = "In the forests of the night\n"

thirdSen = "What immortal hand or eye\n"

fourthSen =
    "Could frame thy fearful\
    \ symmetry?"

sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

testMyLines :: Spec
testMyLines =
    context "myLines" $ do
        it "Returns original content when recombined" $
            intercalate "\n" (myLines sentences) == sentences

spec :: Spec
spec = do
    testMyWords
    testMyLines
