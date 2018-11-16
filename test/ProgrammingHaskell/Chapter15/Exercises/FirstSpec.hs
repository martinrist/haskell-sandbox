module ProgrammingHaskell.Chapter15.Exercises.FirstSpec where

import Test.Hspec
import Data.Monoid
import Test.QuickCheck
import ProgrammingHaskell.Chapter15.Exercises.First
import ProgrammingHaskell.Chapter15.MonoidLaws

type FirstMappend = First' String -> First' String -> First' String -> Bool
type FirstId = First' String -> Bool

testFirst :: Spec
testFirst = context "Monoid instance for First" $ do
    it "Satisfies associativity law" $
        property (monoidAssoc :: FirstMappend)
    it "Satisfies left identity law" $
        property (monoidLeftIdentity :: FirstId)
    it "Satisfies right identity law" $
        property (monoidRightIdentity :: FirstId)

spec :: Spec
spec = testFirst
