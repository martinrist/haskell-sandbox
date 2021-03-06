module RealWorld.Chapter03.ListADT where

data List a
    = Cons a
           (List a)
    | Nil
    deriving (Show)

{-# HLINT ignore fromList "Use foldr" #-}
fromList :: [a] -> List a
fromList (x:xs) = Cons x (fromList xs)
fromList []     = Nil

toList :: List a -> [a]
toList (Cons x y) = x : toList y
toList Nil        = []