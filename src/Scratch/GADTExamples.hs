-- See https://en.wikibooks.org/wiki/Haskell/GADT

module Scratch.GADTExamples where

import Test.Hspec

data Expr
    = I Int             -- integer constants
    | B Bool            -- boolean constants
    | Add Expr Expr     -- add two expressions
    | Mul Expr Expr     -- multiple two expressions
    deriving (Show)

eval :: Expr -> Maybe (Either Int Bool)
eval (I n)                     = Just $ Left n
eval (B b)                     = Just $ Right b
eval (Add e1 e2) = case (eval e1, eval e2) of
                        (Just (Left n1), Just (Left n2)) -> Just $ Left (n1 + n2)
                        _ -> Nothing
eval (Mul e1 e2) = case (eval e1, eval e2) of
                        (Just (Left n1), Just (Left n2)) -> Just $ Left (n1 * n2)
                        _ -> Nothing

test :: IO ()
test = hspec $
    describe "eval Tests" $ do
        context "Simple Add" $ do
            it "Adds integers" $
                eval (I 1 `Add` I 2) `shouldBe` Just (Left 3)
            it "Returns `Nothing` when adding `Bool`s" $
                eval (B False `Add` B True) `shouldBe` Nothing
            it "Returns `Nothing` when adding incompatible types" $ do
                eval (I 1 `Add` B False) `shouldBe` Nothing
                eval (B True `Add` I 3) `shouldBe` Nothing
        context "Simple Mul" $ do
            it "Multiplies integers" $
                eval (I 2 `Mul` I 4) `shouldBe` Just (Left 8)
            it "Returns `Nothing` when multiplying `Bool`s" $
                eval (B False `Mul` B True) `shouldBe` Nothing
            it "Returns `Nothing` when multiplying incompatible types" $ do
                eval (I 1 `Mul` B False) `shouldBe` Nothing
                eval (B True `Mul` I 3) `shouldBe` Nothing
        context "Nested expressions" $ do
            it "Adds nested Adds correctly" $
                eval ((I 2 `Add` I 3) `Add` I 4) `shouldBe` Just (Left 9)
            it "Adds nested Mul correctly" $
                eval ((I 2 `Mul` I 3) `Add` I 4) `shouldBe` Just (Left 10)
            it "Propagates `Nothing` in nested expressions" $ do
                eval ((I 2 `Add` B False) `Add` I 4) `shouldBe` Nothing
                eval ((I 2 `Add` I 3) `Add` B True) `shouldBe` Nothing
                eval ((I 2 `Add` B False) `Mul` I 4) `shouldBe` Nothing
                eval ((I 2 `Mul` B True) `Mul` (I 4 `Add` B True)) `shouldBe` Nothing
