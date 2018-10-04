module ProgrammingHaskell.Chapter09.Exercises.EnumFromToSpec where

import ProgrammingHaskell.Chapter09.Exercises.EnumFromTo
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
    context "eftBool" $
        it "Returns same results as `enumFromTo`" $
        property $ \a b -> eftBool a b == enumFromTo a b
    context "eftOrd Tests" $
        it "Returns same results as `enumFromTo`" $
        property $ \a b -> eftOrd a b == enumFromTo a b
    context "eftInt" $
        it "Returns same results as `enumFromTo`" $
        property $ \a b -> eftInt a b == enumFromTo a b
    context "eftChar" $
        it "Returns same results as `enumFromTo`" $
        property $ \a b -> eftChar a b == enumFromTo a b
