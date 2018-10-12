module ProgrammingHaskell.Chapter12.Exercises.NaturalSpec where

import ProgrammingHaskell.Chapter12.Exercises.Natural
import Test.Hspec

testNatToInteger :: Spec
testNatToInteger =
    context "natToInteger" $ do
        it "Converts Zero to 0" $ natToInteger Zero `shouldBe` 0
        it "Converts Succ Zero to 1" $ natToInteger (Succ Zero) `shouldBe` 1
        it "Converts Succ Succ Zero to 2" $
            natToInteger (Succ (Succ Zero)) `shouldBe` 2

testIntegerToNat :: Spec
testIntegerToNat =
    context "integerToNat" $ do
        it "Converts 0 to Just Zero" $ integerToNat 0 `shouldBe` Just Zero
        it "Converts 1 to Just (Succ Zero)" $
            integerToNat 1 `shouldBe` Just (Succ Zero)
        it "Converts 2 to Just (Succ (Succ Zero))" $
            integerToNat 2 `shouldBe` Just (Succ (Succ Zero))
        it "Converts (-1) to Nothing" $ integerToNat (-1) `shouldBe` Nothing

spec :: Spec
spec = do
    testNatToInteger
    testIntegerToNat
