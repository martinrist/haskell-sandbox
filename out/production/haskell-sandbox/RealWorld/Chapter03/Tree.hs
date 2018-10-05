module RealWorld.Chapter03.Tree where

data Tree a
    = Node a
           (Tree a)
           (Tree a)
    | Empty
    deriving (Show)

data Tree' a =
    Node' a
            (Maybe (Tree' a))
            (Maybe (Tree' a))
    deriving (Show)