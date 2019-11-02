module ThinkingWithTypes.Chapter3.ExercisesSpec where

import           ThinkingWithTypes.Chapter3.Exercises
import           Test.Hspec
import           Test.QuickCheck.Classes
import           Test.Hspec.Checkers

testT1FunctorInstance :: Spec
testT1FunctorInstance =
    describe "Exercise 3-i - `T1 a` Tests" $
        testBatch $ functor (undefined :: T1 (Int, Int, Int))

testT5FunctorInstance :: Spec
testT5FunctorInstance =
    describe "Exercise 3-i - `T5 a` Tests" $
        testBatch $ functor (undefined :: T5 (Int, Int, Int))

spec :: Spec
spec = do
    testT1FunctorInstance
    testT5FunctorInstance
