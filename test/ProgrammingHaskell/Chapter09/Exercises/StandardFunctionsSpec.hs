module ProgrammingHaskell.Chapter09.Exercises.StandardFunctionsSpec where

import Control.Monad (join)
import Data.List (maximumBy, minimumBy)
import ProgrammingHaskell.Chapter09.Exercises.StandardFunctions
import Test.Hspec
import Test.QuickCheck

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
    it "Works the same as the `elem` built-in" $
    property ((\x ys -> myElem x ys == elem x ys) :: Int -> [Int] -> Bool)

testMyReverse :: Spec
testMyReverse =
    context "myReverse" $
    it "Works the same as the `reverse` built-in" $
    property ((\xs -> myReverse xs == reverse xs) :: [Int] -> Bool)

testSquish :: Spec
testSquish =
    context "squich" $
    it "Works the same as Control.Monad.join`" $
    property ((\xss -> squish xss == join xss) :: [[Int]] -> Bool)

testSquishMap :: Spec
testSquishMap =
    context "squishMap" $
    it "Works the same as `>>=`" $
    property
        ((\(Fn f) xs -> squishMap f xs == (xs >>= f)) :: Fun Int [Int] -> [Int] -> Bool)

testSquishAgain :: Spec
testSquishAgain =
    context "squishAgain" $
    it "Works the same as `squish`" $
    property ((\xss -> squishAgain xss == squish xss) :: [[Int]] -> Bool)

-- We can't test these easily using QuickCheck because `maximumBy` is a partial function
-- `maximumBy []` throws an exception so in shrinking the test data, QuickCheck will always
-- end up throwing the exception.  But this is how we'd do it if we could...
-- testMyMaximumBy :: Spec
-- testMyMaximumBy =
--     context "myMaximumBy" $
--     it "Works the same as `maximumBy`" $
--     property
--         ((\(Fn2 f) xs -> myMaximumBy f xs == maximumBy f xs) :: Fun (Int, Int) Ordering -> [Int] -> Bool)
-- testMyMinimumBy :: Spec
-- testMyMinimumBy =
--     context "myMinimumBy" $
--     it "Works the same as `MinimumBy`" $
--     property
--         ((\(Fn2 f) xs -> myMinimumBy f xs == minimumBy f xs) :: Fun (Int, Int) Ordering -> [Int] -> Bool)
spec :: Spec
spec = do
    testMyOr
    testMyAny
    testMyElem
    testMyReverse
    testSquish
    testSquishMap
    testSquishAgain
