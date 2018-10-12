module ProgrammingHaskell.Chapter12.Exercises.EitherSpec where

import ProgrammingHaskell.Chapter12.Exercises.Either
import Test.Hspec

testLefts' :: Spec
testLefts' =
    context "lefts'" $ do
        it "Returns empty list for empty list" $
            lefts' [] `shouldBe` ([] :: [Int])
        it "Returns empty list for single Right" $
            lefts' [Right "foo"] `shouldBe` ([] :: [Int])
        it "Returns single value list for single Left" $
            lefts' [Left 1] `shouldBe` [1]
        it "Filters out multiple Right values" $
            lefts' [Left 1, Right 'b', Left 3, Right 'd'] `shouldBe` [1, 3]

testRights' :: Spec
testRights' =
    context "Rights'" $ do
        it "Returns empty list for empty list" $
            rights' [] `shouldBe` ([] :: [Int])
        it "Returns empty list for single Left" $
            rights' [Left "foo"] `shouldBe` ([] :: [Int])
        it "Returns single value list for single Right" $
            rights' [Right 1] `shouldBe` [1]
        it "Filters out multiple Left values" $
            rights' [Left 1, Right 'b', Left 3, Right 'd'] `shouldBe` ['b', 'd']

testPartitionEithers' :: Spec
testPartitionEithers' =
    context "partitionEithers'" $ do
        it "Returns tuple of two empty lists for empty list" $
            partitionEithers' [] `shouldBe` (([], []) :: ([Int], [Int]))
        it "Returns empty first list if no Lefts" $
            partitionEithers' [Right 'b', Right 'd'] `shouldBe`
            (([], ['b', 'd']) :: ([Int], [Char]))
        it "Returns empty second list if no Rights" $
            partitionEithers' [Left 1, Left 3] `shouldBe`
            (([1, 3], []) :: ([Int], [Int]))
        it "Correctly partitions Lefts and Rights" $
            partitionEithers' [Left 1, Right 'b', Left 3, Right 'd'] `shouldBe`
            ([1, 3], ['b', 'd'])

testEitherMaybe' :: Spec
testEitherMaybe' =
    context "eitherMaybe'" $ do
        it "Returns Nothing if called on Left" $
            eitherMaybe' (+ 1) (Left 1) `shouldBe` Nothing
        it "Applies function if called on Right" $
            eitherMaybe' (+ 1) (Right 1) `shouldBe` Just 2

testEither' :: Spec
testEither' =
    context "either'" $ do
        it "Applies first function if called on Left" $
            either' (+ 1) (length :: String -> Int) (Left 1) `shouldBe` 2
        it "Applies second function if called on Right" $
            either' (+ 1) length (Right "foo") `shouldBe` 3

testEitherMaybe'' :: Spec
testEitherMaybe'' =
    context "eitherMaybe''" $ do
        it "Returns Nothing if called on Left" $
            eitherMaybe'' (+ 1) (Left 1) `shouldBe` Nothing
        it "Applies function if called on Right" $
            eitherMaybe'' (+ 1) (Right 1) `shouldBe` Just 2

spec :: Spec
spec = do
    testLefts'
    testRights'
    testPartitionEithers'
    testEitherMaybe'
    testEither'
    testEitherMaybe''
