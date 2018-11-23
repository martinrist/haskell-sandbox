module ProgrammingHaskell.Chapter15.Exercises.SemigroupSpec where

import           ProgrammingHaskell.Chapter15.Exercises.Semigroup
import           ProgrammingHaskell.Chapter15.SemigroupLaws
import           Test.Hspec
import           Test.QuickCheck         hiding ( Success
                                                , Failure
                                                )
import           Data.Semigroup


-- Question 1 - Trivial

testTrivial :: Spec
testTrivial =
    context "Trivial" $ it "Obeys Semigroup associative law" $ property
        (semigroupAssoc :: AssociativityProp Trivial)


-- Question 2 - Identity

testIdentity :: Spec
testIdentity =
    context "Identity" $ it "Obeys Semigroup associative law" $ property
        (semigroupAssoc :: AssociativityProp (Identity String))


-- Question 3 - Two

testTwo :: Spec
testTwo = context "Two" $ it "Obeys Semigroup associative law" $ property
    (semigroupAssoc :: AssociativityProp (Two String String))


-- Question 4 - Three

testThree :: Spec
testThree = context "Three" $ it "Obeys Semigroup associative law" $ property
    (semigroupAssoc :: AssociativityProp (Three String String String))


-- Question 5 - Four

testFour :: Spec
testFour = context "Four" $ it "Obeys Semigroup associative law" $ property
    (semigroupAssoc :: AssociativityProp (Four String String String String))


-- Question 6 - BoolConj

testBoolConj :: Spec
testBoolConj = context "BoolConj" $ do
    it "Obeys Semigroup associative law" $ property
        (semigroupAssoc :: AssociativityProp BoolConj)
    it "Behaves like conjunction for two `True`s"
        $          BoolConj True
        <>         BoolConj True
        `shouldBe` BoolConj True
    it "Behaves like conjunction for `True` and `False`"
        $          BoolConj True
        <>         BoolConj False
        `shouldBe` BoolConj False


-- Question 7 - BoolDisj

testBoolDisj :: Spec
testBoolDisj = context "BoolDisj" $ do
    it "Obeys Semigroup associative law" $ property
        (semigroupAssoc :: AssociativityProp BoolDisj)
    it "Behaves like disjunction for two `True`s"
        $          BoolDisj True
        <>         BoolDisj True
        `shouldBe` BoolDisj True
    it "Behaves like disjunction for `True` and `False`"
        $          BoolDisj True
        <>         BoolDisj False
        `shouldBe` BoolDisj True


-- Question 8 - Or

testOr :: Spec
testOr = context "Or" $ do
    it "Obeys Semigroup associative law" $ property
        (semigroupAssoc :: AssociativityProp (Or Int String))
    it "Discards `Fst` when followed by `Snd`" $ Fst 1 <> Snd 2 `shouldBe` Snd 2
    it "Discards `Fst` when followed by `Fst`"
        $          Fst 1
        <>         Fst 2
        `shouldBe` (Fst 2 :: Or Int Int)
    it "Keeps `Snd` when followed by `Fst`" $ Snd 1 <> Fst 2 `shouldBe` Snd 1
    it "Keeps `Snd` when followed by `Snd`"
        $          Snd 1
        <>         Snd 2
        `shouldBe` (Snd 1 :: Or Int Int)


-- Question 9 - Combine

f :: Combine Int (Sum Int)
f = Combine $ \n -> Sum (n + 1)

g :: Combine Int (Sum Int)
g = Combine $ \n -> Sum (n - 1)

testCombine :: Spec
testCombine = context "Combine" $ do
    it "Obeys Semigroup associative law" $ property
        (wrappedFnAssoc unCombine :: Int -> AssociativityProp (Combine Int String))
    it "`unCombine (f <> g) 0` returns 0" $ unCombine (f <> g) 0 `shouldBe` Sum
        0
    it "`unCombine (f <> g) 1` returns 2" $ unCombine (f <> g) 1 `shouldBe` Sum
        2
    it "`unCombine (f <> f) 1` returns 4" $ unCombine (f <> f) 1 `shouldBe` Sum
        4
    it "`unCombine (g <> f) 1` returns 2" $ unCombine (g <> f) 1 `shouldBe` Sum
        2


-- Question 10 - Comp

inc :: Comp Int
inc = Comp $ \n -> n + 1

double :: Comp Int
double = Comp $ \n -> n * 2

testComp :: Spec
testComp = context "Comp" $ do
    it "Obeys Semigroup associative law" $ property
        (wrappedFnAssoc unComp :: Int -> AssociativityProp (Comp Int))
    it "`unComp (double <> inc) 3` should return 8`"
        $          unComp (double <> inc) 3
        `shouldBe` 8


-- Question 11 - Valdiation

testValidation :: Spec
testValidation = context "Validation" $ do
    it "Obeys Semigroup associative law" $ property
        (semigroupAssoc :: AssociativityProp (Validation String String))
    it "Retains initial `Success` followed by `Failure`"
        $          Success 1
        <>         Failure "blah"
        `shouldBe` Success 1
    it "Mappends multiple `Failure`s"
        $          Failure "woot"
        <>         Failure "blah"
        `shouldBe` (Failure "wootblah" :: Validation String String)
    it "Retains first `Success` followed by another `Success`"
        $          Success 1
        <>         Success 2
        `shouldBe` (Success 1 :: Validation String Int)
    it "Skips first `Failure` followed by `Success`"
        $          Failure "woot"
        <>         Success 2
        `shouldBe` Success 2

spec :: Spec
spec = do
    testTrivial
    testIdentity
    testTwo
    testThree
    testFour
    testBoolConj
    testBoolDisj
    testOr
    testCombine
    testComp
    testValidation
