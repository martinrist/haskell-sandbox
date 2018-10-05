module ProgrammingHaskell.Chapter09.Exercises.StandardFunctionsSpec where

import           ProgrammingHaskell.Chapter09.Exercises.StandardFunctions
import           Test.Hspec
import           Test.QuickCheck

testMyOr :: Spec
testMyOr =
    context "myOr" $
    it "Works the same as the `or` built-in" $
    property ((\xs -> myOr xs == or xs) :: [Bool] -> Bool)

testMyAny :: Spec
testMyAny =
    context "myAny" $
    it "Works the same as the `any` built-in" $
    property
        ((\(Fn f) xs -> myAny f xs == any f xs) :: Fun Int Bool -> [Int] -> Bool)

testMyElem :: Spec
testMyElem =
    context "myElem" $
    it "Works the same as the `elem` build-in" $
    property ((\x ys -> myElem x ys == elem x ys) :: Int -> [Int] -> Bool)

testMyReverse :: Spec
testMyReverse =
    context "myReverse" $
    it "Works the same as the `reverse` build-in" $
    property ((\xs -> myReverse xs == reverse xs) :: [Int] -> Bool)

spec :: Spec
spec = do
    testMyOr
    testMyAny
    testMyElem
    testMyReverse