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
        property (monoidLeftIdentity :: Trivial -> Bool)
    it "Obeys Monoid right identity law" $
        property (monoidRightIdentity :: Trivial -> Bool)



-- -- Question 2 - Identity

testIdentity :: Spec
testIdentity = context "Identity" $ do
    it "Obeys Monoid left identity law" $
        property (monoidLeftIdentity :: Identity String -> Bool)
    it "Obeys Monoid right identity law" $
        property (monoidRightIdentity :: Identity String -> Bool)


-- Question 3 - Two

testTwo :: Spec
testTwo = context "Two" $ do
    it "Obeys Monoid left identity law" $
        property (monoidLeftIdentity :: Two String String -> Bool)
    it "Obeys Monoid right identity law" $
        property (monoidRightIdentity :: Two String String -> Bool)


-- Question 4 - BoolConj

testBoolConj :: Spec
testBoolConj = context "BoolConj" $ do
    it "Obeys Monoid left identity law" $
        property (monoidLeftIdentity :: BoolConj -> Bool)
    it "Obeys Monoid right identity law" $
        property (monoidRightIdentity :: BoolConj -> Bool)


-- Question 5 - BoolDisj

testBoolDisj :: Spec
testBoolDisj = context "BoolDisj" $ do
    it "Obeys Monoid left identity law" $
        property (monoidLeftIdentity :: BoolDisj -> Bool)
    it "Obeys Monoid right identity law" $
        property (monoidRightIdentity :: BoolDisj -> Bool)


-- Question 6 - Combine

combineLeftIdentity :: (Eq b, Monoid b) => a -> Combine a b -> Bool
combineLeftIdentity v a = unCombine (mempty <> a) v == unCombine a v

combineRightIdentity :: (Eq b, Monoid b) => a -> Combine a b -> Bool
combineRightIdentity v a = unCombine (a <> mempty) v == unCombine a v

testCombine :: Spec
testCombine = context "Combine" $ do
    it "Obeys Monoid left identity law" $
        property (combineLeftIdentity :: Int -> Combine Int String -> Bool)
    it "Obeys Monoid right identity law" $
        property (combineRightIdentity :: Int -> Combine Int String -> Bool)


-- Question 7 - Comp

compLeftIdentity :: Eq a => a -> Comp a -> Bool
compLeftIdentity v a = unComp (mempty <> a) v == unComp a v

compRightIdentity :: Eq a => a -> Comp a -> Bool
compRightIdentity v a = unComp (a <> mempty) v == unComp a v

testComp :: Spec
testComp = context "Comp" $ do
    it "Obeys Monoid left identity law" $
        property (compLeftIdentity :: Int -> Comp Int -> Bool)
    it "Obeys Monoid right identity law" $
        property (compRightIdentity :: Int -> Comp Int -> Bool)


-- Question 8 - Mem

f' = Mem $ \s -> ("hi", s + 1)


memAssoc :: (Eq s, Eq a, Semigroup a, Monoid a) => s -> Mem s a -> Mem s a -> Mem s a -> Bool
memAssoc v a b c =     runMem (a <> (b <> c)) v
                    == runMem ((a <> b) <> c) v

memLeftIdentity :: (Eq s, Eq a, Semigroup a, Monoid a) => s -> Mem s a -> Bool
memLeftIdentity v m = runMem (mempty <> m) v == runMem m v

memRightIdentity :: (Eq s, Eq a, Semigroup a, Monoid a) => s -> Mem s a -> Bool
memRightIdentity v m = runMem (m <> mempty) v == runMem m v

testMem :: Spec
testMem = context "Mem" $ do
    it "Obeys Semigroup associative law" $
        property (memAssoc :: Int -> Mem Int String -> Mem Int String -> Mem Int String -> Bool)
    it "Obeys Monoid left identity law" $
        property (memLeftIdentity :: Int -> Mem Int String -> Bool)
    it "Obeys Monoid right identity law" $
        property (memRightIdentity :: Int -> Mem Int String -> Bool)
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
