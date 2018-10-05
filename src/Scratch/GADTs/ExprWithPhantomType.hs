-- See https://en.wikibooks.org/wiki/Haskell/GADT
module Scratch.GADTs.ExprWithPhantomType where

import Test.Hspec

data Expr a
    = I Int -- integer constants
    | B Bool -- boolean constants
    | Add (Expr a)
          (Expr a) -- add two expressions
    | Mul (Expr a)
          (Expr a) -- multiply two expressions
    | Eq (Expr a)
         (Expr a) -- test equality
    deriving (Show)

add :: Expr Int -> Expr Int -> Expr Int
add = Add

mul :: Expr Int -> Expr Int -> Expr Int
mul = Mul

eq :: Expr Int -> Expr Int -> Expr Int
eq = Eq

i :: Int -> Expr Int
i = I

b :: Bool -> Expr Bool
b = B

eval :: Expr a -> a
-- This doesn't compile because the compiler can't know that a = Int
--eval (I n) = n
eval = undefined
