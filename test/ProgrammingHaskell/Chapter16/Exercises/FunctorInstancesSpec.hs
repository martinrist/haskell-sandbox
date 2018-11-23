module ProgrammingHaskell.Chapter16.Exercises.FunctorInstancesSpec where

import           ProgrammingHaskell.Chapter16.Exercises.FunctorInstances
import           ProgrammingHaskell.Chapter16.FunctorLaws
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Function
import           GHC.Arr


-- Question 2 - BoolAndSomethingElse

testBoolAndSomethingElse :: Spec
testBoolAndSomethingElse = context "BoolAndSomethingElse" $ do
    it "Obeys Functor identity law"
        $ property (functorIdentity :: FunctorIdentity BoolAndSomethingElse Int)
    it "Obeys Functor composition law" $ property
        (functorCompose :: FunctorCompose BoolAndSomethingElse Int String Int)


-- Question 3 - BoolAndMaybeSomethingElse

testBoolAndMaybeSomethingElse :: Spec
testBoolAndMaybeSomethingElse = context "BoolAndMaybeSomethingElse" $ do
            it "Obeys Functor identity law"
                $ property (functorIdentity :: FunctorIdentity BoolAndMaybeSomethingElse Int)
            it "Obeys Functor composition law" $ property
                (functorCompose :: FunctorCompose BoolAndMaybeSomethingElse Int String Int)



spec :: Spec
spec = do
    testBoolAndSomethingElse
    testBoolAndMaybeSomethingElse
