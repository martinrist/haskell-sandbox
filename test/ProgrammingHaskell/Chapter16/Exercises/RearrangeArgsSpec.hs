{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}

module ProgrammingHaskell.Chapter16.Exercises.RearrangeArgsSpec where

import           ProgrammingHaskell.Chapter16.Exercises.RearrangeArgs
import           ProgrammingHaskell.Chapter16.FunctorLaws
import           Test.Hspec
import           Test.QuickCheck


-- Question 1 - Sum

testSum :: Spec
testSum = context "Sum" $ do
    it "Obeys Functor identity law"
        $ property (functorIdentity :: FunctorIdentity (Sum String) Int)
    it "Obeys Functor composition law" $ property
        (functorCompose :: FunctorCompose (Sum String) Int String Int)


-- Question 2 - Company

testCompany :: Spec
testCompany = context "Company" $ do
    it "Obeys Functor identity law"
        $ property (functorIdentity :: FunctorIdentity (Company Int String) Int)
    it "Obeys Functor composition law" $ property
        (functorCompose :: FunctorCompose (Company Int String) Int String Int)


-- Question 3 - More

testMore :: Spec
testMore = context "More" $ do
    it "Obeys Functor identity law"
        $ property (functorIdentity :: FunctorIdentity (More Int) Int)
    it "Obeys Functor composition law" $ property
        (functorCompose :: FunctorCompose (More Int) Int String Int)


spec :: Spec
spec = do
    testSum
    testCompany
    testMore