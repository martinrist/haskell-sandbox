{-# LANGUAGE TypeOperators #-}

module ProgrammingHaskell.Chapter15.Exercises.SemigroupSpec where

import           ProgrammingHaskell.Chapter15.Exercises.Semigroup
import           ProgrammingHaskell.Chapter15.SemigroupLaws
import           Test.Hspec
import           Test.QuickCheck

testTrivial :: Spec
testTrivial =
    context "Trivial" $ it "Obeys Semigroup associative law" $ property
        (semigroupAssoc :: Trivial -> Trivial -> Trivial -> Bool)

testIdentity :: Spec
testIdentity =
    context "Identity" $ it "Obeys Semigroup associative law" $ property
        (semigroupAssoc :: Identity Int -> Identity Int -> Identity Int -> Bool)

testTwo :: Spec
testTwo = context "Two" $ it "Obeys Semigroup associative law" $ property
    (semigroupAssoc :: Two String String
      -> Two String String
      -> Two String String
      -> Bool
    )

testThree :: Spec
testThree = context "Three" $ it "Obeys Semigroup associative law" $ property
    (semigroupAssoc :: Three String String String
      -> Three String String String
      -> Three String String String
      -> Bool
    )

testFour :: Spec
testFour = context "Four" $ it "Obeys Semigroup associative law" $ property
    (semigroupAssoc :: Four String String String String
      -> Four String String String String
      -> Four String String String String
      -> Bool
    )

testBoolConj :: Spec
testBoolConj = context "BoolConj" $
    it "Obeys Semigroup associative law" $
        property (semigroupAssoc :: BoolConj -> BoolConj -> BoolConj -> Bool)

testBoolDisj :: Spec
testBoolDisj = context "BoolDisj" $
    it "Obeys Semigroup associative law" $
        property (semigroupAssoc :: BoolDisj -> BoolDisj -> BoolDisj -> Bool)

spec :: Spec
spec = do
    testTrivial
    testIdentity
    testTwo
    testThree
    testFour
    testBoolConj
    testBoolDisj
