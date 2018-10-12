module ProgrammingHaskell.Chapter12.Exercises.StringProcessingSpec where

import ProgrammingHaskell.Chapter12.Exercises.StringProcessing
import Test.Hspec
import Test.QuickCheck

testNotThe :: Spec
testNotThe =
    context "notThe" $ do
        it "Returns `Nothing` for `the`" $ notThe "the" `shouldBe` Nothing
        it "Returns `Just word` for `word` containing `the`" $
            notThe "blahtheblah" `shouldBe` Just "blahtheblah"
        it "Returns `Just word` for `word` without `the`" $
            notThe "woot" `shouldBe` Just "woot"

testReplaceThe :: Spec
testReplaceThe =
    context "replaceThe" $ do
        it "Replaces `the` in sentence containing single `the` at start" $
            replaceThe "the cow loves us" `shouldBe` "a cow loves us"
        it "Replaces `the` in sentence containing single `the` in middle" $
            replaceThe "we love the cow" `shouldBe` "we love a cow"
        it "Replaces `the` in sentence containing multiple `the`s" $
            replaceThe "the cow loves the other cow" `shouldBe`
            "a cow loves a other cow"

testCountTheBeforeVowel :: Spec
testCountTheBeforeVowel =
    context "countTheBeforeVowel" $ do
        it "Returns 0 for sentence with no `the`" $
            countTheBeforeVowel "doesn't have magic word" `shouldBe` 0
        it "Returns 0 for `the` without following vowel" $
            countTheBeforeVowel "the cow" `shouldBe` 0
        it "Returns 1 for `the` with following vowel" $
            countTheBeforeVowel "the evil cow" `shouldBe` 1

testCountVowels :: Spec
testCountVowels =
    context "countVowels" $ do
        it "Returns 0 for empty string" $ countVowels "" `shouldBe` 0
        it "Returns 1 for single vowel" $ countVowels "a" `shouldBe` 1
        it "Returns correct count for multiple words" $
            countVowels "the cow" `shouldBe` 2
        it "Returns correct count for longer word" $
            countVowels "Mikolajczak" `shouldBe` 4

spec :: Spec
spec = do
    testNotThe
    testReplaceThe
    testCountTheBeforeVowel
    testCountVowels
