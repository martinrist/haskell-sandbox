module ProgrammingHaskell.Chapter16.Exercises.FunctorSpec where

import           ProgrammingHaskell.Chapter15.Exercises.Semigroup
import           ProgrammingHaskell.Chapter16.Exercises.Functor
import           ProgrammingHaskell.Chapter16.FunctorLaws
import           Test.Hspec
import           Test.QuickCheck         hiding ( Success
                                                , Failure
                                                )
import           Test.QuickCheck.Function
import           Data.Semigroup


-- Question 1 - Identity

testIdentity :: Spec
testIdentity = context "Identity" $ do
    it "Obeys Functor identity law"
        $ property (functorIdentity :: FunctorIdentity Identity Int)
    it "Obeys Functor composition law" $ property
        (functorCompose :: FunctorCompose Identity Int String Int)


-- Question 2 - Pair

testPair :: Spec
testPair = context "Pair" $ do
    it "Obeys Functor identity law"
        $ property (functorIdentity :: FunctorIdentity Pair Int)
    it "Obeys Functor composition law"
        $ property (functorCompose :: FunctorCompose Pair Int String Int)


-- Question 3 - Two

testTwo :: Spec
testTwo = context "Two" $ do
    it "Obeys Functor identity law"
        $ property (functorIdentity :: FunctorIdentity (Two String) Int)
    it "Obeys Functor composition law" $ property
        (functorCompose :: FunctorCompose (Two String) Int String Int)


-- Question 4 - Three

testThree :: Spec
testThree = context "Three" $ do
    it "Obeys Functor identity law" $ property
        (functorIdentity :: FunctorIdentity (Three String String) Int)
    it "Obeys Functor composition law"
        $ property
              (functorCompose :: FunctorCompose
                    (Three String String)
                    Int
                    String
                    Int
              )


-- Question 5 - Three'

testThree' :: Spec
testThree' = context "Three'" $ do
    it "Obeys Functor identity law"
        $ property (functorIdentity :: FunctorIdentity (Three' String) Int)
    it "Obeys Functor composition law" $ property
        (functorCompose :: FunctorCompose (Three' String) Int String Int)


-- Question 6 - Four

testFour :: Spec
testFour = context "Four" $ do
    it "Obeys Functor identity law" $ property
        (functorIdentity :: FunctorIdentity (Four String String String) Int)
    it "Obeys Functor composition law" $ property
        (functorCompose :: FunctorCompose
              (Four String String String)
              Int
              String
              Int
        )


-- Question 7 - Four'

testFour' :: Spec
testFour' = context "Four'" $ do
    it "Obeys Functor identity law"
        $ property (functorIdentity :: FunctorIdentity (Four' String) Int)
    it "Obeys Functor composition law" $ property
        (functorCompose :: FunctorCompose (Four' String) Int String Int)

spec :: Spec
spec = do
    testIdentity
    testPair
    testTwo
    testThree
    testThree'
    testFour
    testFour'
