module ProgrammingHaskell.Chapter14.Exercises.WordNumberSpec where

import ProgrammingHaskell.Chapter08.Exercises.WordNumber
import Test.Hspec

testDigitToWord :: Spec
testDigitToWord = describe "digitToWord" $ do
    it "returns zero for 0" $
        digitToWord 0 `shouldBe` "zero"
    it "returns one for 1" $
        digitToWord 1 `shouldBe` "one"

testDigits :: Spec
testDigits = describe "digits" $ do
    it "returns [1] for 1" $
        digits 1 `shouldBe` [1]
    it "returns [0] for 0" $
        digits 0 `shouldBe` [0]
    it "returns [1, 0] for 10" $
        digits 10 `shouldBe` [1, 0]
    it "returns [1, 0, 1] for 101" $
        digits 101 `shouldBe` [1, 0, 1]

testWordNumber :: Spec
testWordNumber = describe "wordNumber" $ do
    it "returns one-zero-zero given 100" $
        wordNumber 100 `shouldBe` "one-zero-zero"
    it "returns nine-zero-zero-one given 9001" $
        wordNumber 9001 `shouldBe` "nine-zero-zero-one"

spec :: Spec
spec = do
    testDigitToWord
    testDigits
    testWordNumber