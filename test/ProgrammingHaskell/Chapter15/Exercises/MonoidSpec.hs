module ProgrammingHaskell.Chapter15.Exercises.MonoidSpec where

import           ProgrammingHaskell.Chapter15.Exercises.Monoid
import           ProgrammingHaskell.Chapter15.Exercises.Semigroup
import           ProgrammingHaskell.Chapter15.MonoidLaws
import           Test.Hspec
import           Test.QuickCheck         hiding ( Success
                                                , Failure
                                                )
import           Data.Semigroup


-- Question 1 - Trivial

testTrivial :: Spec
testTrivial = context "Trivial" $ do
    it "Obeys Monoid left identity law" $
        property (monoidLeftIdentity :: IdentityProp Trivial)
    it "Obeys Monoid right identity law" $
        property (monoidRightIdentity :: IdentityProp Trivial)



-- -- Question 2 - Identity

testIdentity :: Spec
testIdentity = context "Identity" $ do
    it "Obeys Monoid left identity law" $
        property (monoidLeftIdentity :: IdentityProp String)
    it "Obeys Monoid right identity law" $
        property (monoidRightIdentity :: IdentityProp String)


-- Question 3 - Two

testTwo :: Spec
testTwo = context "Two" $ do
    it "Obeys Monoid left identity law" $
        property (monoidLeftIdentity :: IdentityProp (Two String String))
    it "Obeys Monoid right identity law" $
        property (monoidRightIdentity :: IdentityProp (Two String String))


-- Question 4 - BoolConj

testBoolConj :: Spec
testBoolConj = context "BoolConj" $ do
    it "Obeys Monoid left identity law" $
        property (monoidLeftIdentity :: IdentityProp BoolConj)
    it "Obeys Monoid right identity law" $
        property (monoidRightIdentity :: IdentityProp BoolConj)


-- Question 5 - BoolDisj

testBoolDisj :: Spec
testBoolDisj = context "BoolDisj" $ do
    it "Obeys Monoid left identity law" $
        property (monoidLeftIdentity :: IdentityProp BoolDisj)
    it "Obeys Monoid right identity law" $
        property (monoidRightIdentity :: IdentityProp BoolDisj)


-- Question 6 - Combine

testCombine :: Spec
testCombine = context "Combine" $ do
    it "Obeys Monoid left identity law" $
        property (wrappedFnLeftIdentity unCombine :: Int -> IdentityProp (Combine Int String))
    it "Obeys Monoid right identity law" $
        property (wrappedFnRightIdentity unCombine :: Int -> IdentityProp (Combine Int String))


-- Question 7 - Comp

testComp :: Spec
testComp = context "Comp" $ do
    it "Obeys Monoid left identity law" $
        property (wrappedFnLeftIdentity unComp :: Int -> IdentityProp (Comp Int))
    it "Obeys Monoid right identity law" $
        property (wrappedFnRightIdentity unComp :: Int -> IdentityProp (Comp Int))


-- Question 8 - Mem

f' = Mem $ \s -> ("hi", s + 1)

testMem :: Spec
testMem = context "Mem" $ do
    it "Obeys Semigroup associative law" $
        property (wrappedFnAssoc runMem :: Int -> AssociativityProp (Mem Int String))
    it "Obeys Monoid left identity law" $
        property (wrappedFnLeftIdentity runMem :: Int -> IdentityProp (Mem Int String))
    it "Obeys Monoid right identity law" $
        property (wrappedFnRightIdentity runMem :: Int -> IdentityProp (Mem Int String))
    it "`runMem empty 0` returns `('', 0)`" $
        runMem mempty 0 `shouldBe` ( ("", 0) :: (String, Int) )
    it "`runMem (f' <> mempty) 0` returns `('hi', 1)`" $
        runMem (f' <> mempty) 0 `shouldBe` ("hi", 1)
    it "`runMem (mempty <> f') 0` returns `('hi', 1)`" $
        runMem (mempty <> f') 0 `shouldBe` ("hi", 1)

spec :: Spec
spec = do
    testTrivial
    testIdentity
    testTwo
    testBoolConj
    testBoolDisj
    testCombine
    testComp
    testMem
