module ProgrammingHaskell.Chapter06.Exercises.WhatCanWeDo where

data Rocks =
    Rocks String
    deriving (Eq, Show, Ord)

data Yeah =
    Yeah Bool
    deriving (Eq, Show, Ord)

data Papu =
    Papu Rocks
         Yeah
    deriving (Eq, Show, Ord)

-- Question 1
--phew = Papu "chases" True
phew :: Papu
phew = Papu (Rocks "chases") (Yeah True)

-- Question 2
truth :: Papu
truth = Papu (Rocks "chomskydoz") (Yeah True)

-- Question 3
equalityForAll :: Papu -> Papu -> Bool
equalityForAll p p' = p == p'

-- Question 4
comparePapus :: Papu -> Papu -> Bool
comparePapus p p' = p > p'