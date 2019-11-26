module ProgrammingInHaskell.Chapter4Spec where

import           ProgrammingInHaskell.Chapter4
import           Test.Hspec

testHalve :: SpecWith ()
testHalve =
    describe "Exercise 4.1 - `halve`" $ do
        it "Correctly halves an empty list" $
            halve ([] :: [Int]) `shouldBe` ([], [])
        it "Correctly halves a single element list" $
            halve [1] `shouldBe` ([], [1])
        it "Correctly halves a two element list" $
            halve [1, 2] `shouldBe` ([1], [2])
        it "Correctly halves a larger odd-numbered element list" $
            halve [1..11] `shouldBe` ([1..5], [6..11])

testThird :: SpecWith ()
testThird =
    describe "Exercise 4.2 - `third`" $ do
        it "Works with implementation using `head` and `tail`" $
            thirdUsingHeadAndTail [1, 2, 3] `shouldBe` 3
        it "Works with implementation using list indexing" $
            thirdUsingListIndexing [1, 2, 3] `shouldBe` 3
        it "Works with implementation using pattern matching" $
            thirdUsingPatternMatching ([1, 2, 3] :: [Int]) `shouldBe` 3

testSafetail :: SpecWith ()
testSafetail =
    describe "Exercise 4.3 - `safetail`" $ do
        it "Works with implementation using conditionals" $ do
            safetailUsingConditionals ([] :: [Int]) `shouldBe` []
            safetailUsingConditionals [1] `shouldBe` []
            safetailUsingConditionals [1, 2] `shouldBe` [2]
        it "Works with implementation using guards" $ do
            safetailUsingGuards ([] :: [Int]) `shouldBe` []
            safetailUsingGuards [1] `shouldBe` []
            safetailUsingGuards [1, 2] `shouldBe` [2]
        it "Works with implementation using pattern matching" $ do
            safetailUsingPatternMatching ([] :: [Int]) `shouldBe` []
            safetailUsingPatternMatching [1] `shouldBe` []
            safetailUsingPatternMatching [1, 2] `shouldBe` [2]

testDisjunction :: SpecWith ()
testDisjunction =
    describe "Exercise 4.4 - `||` using pattern matching" $
        it "Works as expected" $ do
            False ProgrammingInHaskell.Chapter4.|| False `shouldBe` False
            False ProgrammingInHaskell.Chapter4.|| True `shouldBe` True
            True ProgrammingInHaskell.Chapter4.|| False `shouldBe` True
            True ProgrammingInHaskell.Chapter4.|| True `shouldBe` True

spec :: Spec
spec = do
    testHalve
    testThird
    testSafetail
    testDisjunction