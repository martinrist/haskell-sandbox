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
        $ property (functorIdentity :: Identity Int -> Bool)
    it "Obeys Functor composition law" $ property
        (functorCompose :: Identity Int
          -> Fun Int String
          -> Fun String Int
          -> Bool
        )


-- Question 2 - Pair

testPair :: Spec
testPair = context "Pair" $ do
    it "Obeys Functor identity law"
        $ property (functorIdentity :: Pair Int -> Bool)
    it "Obeys Functor composition law" $
        property
        (functorCompose :: Pair Int
          -> Fun Int String
          -> Fun String Int
          -> Bool
        )


-- Question 3 - Two

testTwo :: Spec
testTwo = context "Two" $ do
    it "Obeys Functor identity law"
        $ property (functorIdentity :: Two String Int -> Bool)
    it "Obeys Functor composition law" $
        property
        (functorCompose :: Two String Int
          -> Fun Int String
          -> Fun String Int
          -> Bool
        )


-- Question 4 - Three

testThree :: Spec
testThree = context "Three" $ do
    it "Obeys Functor identity law"
        $ property (functorIdentity :: Three String String Int -> Bool)
    it "Obeys Functor composition law" $
        property
            (functorCompose :: Three String String Int
              -> Fun Int String
              -> Fun String Int
              -> Bool
            )


-- Question 5 - Three'

testThree' :: Spec
testThree' = context "Three'" $ do
    it "Obeys Functor identity law"
        $ property (functorIdentity :: Three' String Int -> Bool)
    it "Obeys Functor composition law" $
        property
        (functorCompose :: Three' String Int
          -> Fun Int String
          -> Fun String Int
          -> Bool
        )


-- Question 6 - Four

testFour :: Spec
testFour = context "Four" $ do
    it "Obeys Functor identity law" $ property
        (functorIdentity :: Four String String String Int -> Bool)
    it "Obeys Functor composition law" $
        property
        (functorCompose :: Four String String String Int
          -> Fun Int String
          -> Fun String Int
          -> Bool
        )


-- Question 7 - Four'

testFour' :: Spec
testFour' = context "Four'" $ do
    it "Obeys Functor identity law"
        $ property (functorIdentity :: Four' String Int -> Bool)
    it "Obeys Functor composition law" $
        property
        (functorCompose :: Four' String Int
          -> Fun Int String
          -> Fun String Int
          -> Bool
        )


spec :: Spec
spec = do
    testIdentity
    testPair
    testTwo
    testThree
    testThree'
    testFour
    testFour'
