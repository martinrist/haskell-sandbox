module ThinkingWithTypes.Chapter3.ExercisesSpec where

import           ThinkingWithTypes.Chapter3.Exercises
import           Test.Hspec
import           Test.QuickCheck.Classes
import           Test.Hspec.Checkers

testT1FunctorInstance :: Spec
testT1FunctorInstance =
    testBatch $ functor (undefined :: T1 (Int, Int, Int))

spec :: Spec
spec = testT1FunctorInstance