module RealWorld.Chapter04.ExercisesSpec where

import RealWorld.Chapter04.Exercises
import Test.Hspec

testExercise1 :: Spec
testExercise1 =
    context "Exercise 1 Tests" $ do
        testSafeHead
        testSafeTail
        testSafeLast
        testSafeInit

testSafeHead :: Spec
testSafeHead =
    context "safeHead" $ do
        it "works with empty list" $ safeHead ([] :: [()]) `shouldBe` Nothing
        it "works with single element list" $ safeHead ['a'] `shouldBe` Just 'a'
        it "works with two element list" $ safeHead "hi" `shouldBe` Just 'h'

testSafeTail :: Spec
testSafeTail =
    context "safeTail" $ do
        it "works with empty list" $ safeTail ([] :: [()]) `shouldBe` Nothing
        it "works with single element list" $ safeTail "a" `shouldBe` Just ""
        it "works with two element list" $ safeTail "hi" `shouldBe` Just "i"
        it "works with three element list" $ safeTail "foo" `shouldBe` Just "oo"

testSafeLast :: Spec
testSafeLast =
    context "safeLast" $ do
        it "works with empty list" $ safeLast ([] :: [()]) `shouldBe` Nothing
        it "works with single element list" $ safeLast "a" `shouldBe` Just 'a'
        it "works with two element list" $ safeLast "hi" `shouldBe` Just 'i'
        it "works with three element list" $ safeLast "foo" `shouldBe` Just 'o'

testSafeInit :: Spec
testSafeInit =
    context "safeInit" $ do
        it "works with empty list" $ safeInit ([] :: [()]) `shouldBe` Nothing
        it "works with single element list" $ safeInit "a" `shouldBe` Just ""
        it "works with two element list" $ safeInit "hi" `shouldBe` Just "h"
        it "works with three element list" $ safeInit "foo" `shouldBe` Just "fo"

testExercise2 :: Spec
testExercise2 = context "Exercise 2 Tests" $ testSplitWith

testSplitWith :: Spec
testSplitWith =
    context "splitWith" $
    it "works" $
    splitWith (\x -> x `mod` 3 == 0) [1 .. 10] `shouldBe` [[], [3], [6], [9]]

spec :: Spec
spec = do
    testExercise1
    testExercise2
