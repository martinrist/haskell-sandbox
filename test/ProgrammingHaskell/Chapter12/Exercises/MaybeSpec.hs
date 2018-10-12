module ProgrammingHaskell.Chapter12.Exercises.MaybeSpec where

import ProgrammingHaskell.Chapter12.Exercises.Maybe
import Test.Hspec

testIsJust :: Spec
testIsJust =
    context "isJust" $ do
        it "Returns True for Just x" $ isJust (Just 1) `shouldBe` True
        it "Returns False for Nothing" $ isJust Nothing `shouldBe` False

testIsNothing :: Spec
testIsNothing =
    context "isNothing" $ do
        it "Returns True for Nothing" $ isNothing Nothing `shouldBe` True
        it "Returns False for Just x" $ isNothing (Just 1) `shouldBe` False

testMayybee :: Spec
testMayybee =
    context "mayybee" $ do
        it "Returns default value if called with Nothing" $
            mayybee 0 (+ 1) Nothing `shouldBe` 0
        it "Applies function to x if called with Just x" $
            mayybee 0 (+ 1) (Just 1) `shouldBe` 2

testFromMaybe :: Spec
testFromMaybe =
    context "fromMaybe" $ do
        it "Returns default value if called with Nothing" $
            fromMaybe "a" Nothing `shouldBe` "a"
        it "Returns x if called with Just x" $
            fromMaybe "a" (Just "foo") `shouldBe` "foo"

testListToMaybe :: Spec
testListToMaybe =
    context "listToMaybe" $ do
        it "Returns Just the head of the list for non-empty list" $
            listToMaybe [1, 2, 3] `shouldBe` Just 1
        it "Returns Nothing for empty list" $
            listToMaybe ([] :: [Int]) `shouldBe` Nothing

testMaybeToList :: Spec
testMaybeToList =
    context "maybeToList" $ do
        it "Returns single element list for Just x" $
            maybeToList (Just 1) `shouldBe` [1]
        it "Returns empty list for Nothing" $
            maybeToList Nothing `shouldBe` ([] :: [Int])

testCatMaybes :: Spec
testCatMaybes =
    context "catMaybes" $ do
        it "Works on an empty list" $ catMaybes [] `shouldBe` ([] :: [Int])
        it "Returns empty list for list full of Nothing" $
            catMaybes [Nothing, Nothing, Nothing] `shouldBe` ([] :: [Int])
        it "Removes single Nothing at start" $
            catMaybes [Nothing, Just 2, Just 3] `shouldBe` [2, 3]
        it "Removes single Nothing in middle" $
            catMaybes [Just 1, Nothing, Just 3] `shouldBe` [1, 3]
        it "Removes single Nothing at end" $
            catMaybes [Just 1, Just 2, Nothing] `shouldBe` [1, 2]
        it "Removes multiple Nothings" $
            catMaybes [Just 1, Nothing, Nothing, Just 4] `shouldBe` [1, 4]

testFlipMaybe :: Spec
testFlipMaybe =
    context "flipMaybe" $ do
        it "Returns Nothing if any element is Nothing" $
            flipMaybe [Just 1, Nothing] `shouldBe` Nothing
        it "Returns Just [a] if all elements are Justs" $
            flipMaybe [Just 1, Just 2, Just 3] `shouldBe` Just [1, 2, 3]

spec :: Spec
spec = do
    testIsJust
    testIsNothing
    testMayybee
    testFromMaybe
    testListToMaybe
    testMaybeToList
    testCatMaybes
    testFlipMaybe
