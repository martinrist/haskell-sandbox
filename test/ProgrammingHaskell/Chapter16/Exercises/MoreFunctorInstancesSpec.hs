module ProgrammingHaskell.Chapter16.Exercises.MoreFunctorInstancesSpec where

import           ProgrammingHaskell.Chapter16.Exercises.MoreFunctorInstances
import           ProgrammingHaskell.Chapter16.FunctorLaws
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Function


-- Question 1 - Quant

testQuant :: Spec
testQuant = context "Quant" $ do
    it "Obeys Functor identity law" $ property
        (functorIdentity :: FunctorIdentity (Quant Int) Int)
    it "Obeys Functor composition law" $ property
        (functorCompose :: FunctorCompose (Quant Int) Int String Int)


-- Question 2 - K

testK :: Spec
testK = context "K" $ do
    it "Obeys Functor identity law" $ property
        (functorIdentity :: FunctorIdentity (K Int) Int)
    it "Obeys Functor composition law" $ property
        (functorCompose :: FunctorCompose (K Int) Int String Int)


-- Question 3 - Flip

testFlip :: Spec
testFlip = context "Flip" $ do
    it "Obeys Functor identity law" $ property
        (functorIdentity :: FunctorIdentity (Flip L String) Int)
    it "Obeys Functor composition law" $ property
        (functorCompose :: FunctorCompose (Flip L String) Int String Int)


-- Question 4 - EvilGoateeConst

testEvilGoateeConst :: Spec
testEvilGoateeConst = context "EvilGoateeConst" $ do
    it "Obeys Functor identity law" $ property
        (functorIdentity :: FunctorIdentity (EvilGoateeConst Int) Int)
    it "Obeys Functor composition law" $ property
        (functorCompose :: FunctorCompose (EvilGoateeConst Int) Int String Int)


-- Question 5 - LiftItOut

testLiftItOut :: Spec
testLiftItOut = context "LiftItOut" $ do
    it "Obeys Functor identity law" $ property
        (functorIdentity :: FunctorIdentity (LiftItOut Maybe) Int)
    it "Obeys Functor composition law" $ property
        (functorCompose :: FunctorCompose (LiftItOut Maybe) Int String Int)


-- Question 6 - Parappa

testParappa :: Spec
testParappa = context "Parappa" $ do
    it "Obeys Functor identity law" $ property
        (functorIdentity :: FunctorIdentity (Parappa Maybe []) Int)
    it "Obeys Functor composition law" $ property
        (functorCompose :: FunctorCompose (Parappa Maybe []) Int String Int)


-- Question 7 - IgnoreOne

testIgnoreOne :: Spec
testIgnoreOne = context "IgnoreOne" $ do
    it "Obeys Functor identity law" $ property
        (functorIdentity :: FunctorIdentity (IgnoreOne Maybe [] Int) Int)
    it "Obeys Functor composition law" $ property
        (functorCompose :: FunctorCompose (IgnoreOne Maybe [] Int) Int String Int)


-- Question 8 - Notorious

testNotorious :: Spec
testNotorious = context "Notorious" $ do
    it "Obeys Functor identity law" $ property
        (functorIdentity :: FunctorIdentity (Notorious Maybe Int Int) Int)
    it "Obeys Functor composition law" $ property
        (functorCompose :: FunctorCompose (Notorious Maybe Int Int) Int String Int)


-- Question 9 - List

testList :: Spec
testList = context "List" $ do
    it "Obeys Functor identity law" $ property
        (functorIdentity :: FunctorIdentity List Int)
    it "Obeys Functor composition law" $ property
        (functorCompose :: FunctorCompose List Int String Int)


-- Question 10 - GoatLord

testGoatLord :: Spec
testGoatLord = context "GoatLord" $ do
    it "Obeys Functor identity law" $ property
        (functorIdentity :: FunctorIdentity GoatLord Int)
    it "Obeys Functor composition law" $ property
        (functorCompose :: FunctorCompose GoatLord Int String Int)


spec :: Spec
spec = do
    testQuant
    testK
    testFlip
    testEvilGoateeConst
    testLiftItOut
    testParappa
    testIgnoreOne
    testNotorious
    testList
    testGoatLord
