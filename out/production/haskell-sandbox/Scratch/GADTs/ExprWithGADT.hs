-- See https://en.wikibooks.org/wiki/Haskell/GADT
{-# LANGUAGE GADTs #-}
module Scratch.GADTs.ExprWithGADT where

import Test.Hspec

data Expr a where
    I   :: Int  -> Expr Int
    B   :: Bool -> Expr Bool
    Add :: Expr Int -> Expr Int -> Expr Int
    Mul :: Expr Int -> Expr Int -> Expr Int
    Eq  :: Eq a => Expr a -> Expr a -> Expr Bool


eval :: Expr a -> a
-- Here the compiler can now infer that a = Int when encountering the `I` constructor, so it's legal to return `n :: Int`
eval (I n) = n
eval (B b) = b
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Eq  e1 e2) = eval e1 == eval e2

test :: IO ()
test = hspec $
    describe "eval Tests" $ do
        context "Simple Add" $
            it "Adds integers" $ eval (I 1 `Add` I 2) `shouldBe` 3
        context "Simple Mul" $
            it "Multiplies integers" $
                eval (I 2 `Mul` I 4) `shouldBe` 8
        context "Simple Eq" $
            it "Tests boolean equality" $ do
                eval (B False `Eq` B False) `shouldBe` True
                eval (B False `Eq` B True) `shouldBe` False
                eval (B True `Eq` B True) `shouldBe` True
        context "Nested expressions" $ do
            it "Adds nested Adds correctly" $
                eval ((I 2 `Add` I 3) `Add` I 4) `shouldBe` 9
            it "Adds nested Mul correctly" $
                eval ((I 2 `Mul` I 3) `Add` I 4) `shouldBe` 10


