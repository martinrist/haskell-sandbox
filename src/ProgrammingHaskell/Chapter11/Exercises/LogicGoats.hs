{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module ProgrammingHaskell.Chapter11.Exercises.LogicGoats where

class TooMany a where
    tooMany :: a -> Bool

instance TooMany Int where
    tooMany n = n > 42

newtype Goats =
    Goats Int
    deriving (Eq, Show, TooMany)

-- Question 1
instance TooMany (Int, String) where
    tooMany (i, _) = tooMany i

-- Question 2
instance TooMany (Int, Int) where
    tooMany (i, j) = tooMany (i + j)

-- Question 3
instance (Num a, TooMany a) => TooMany (a, a) where
    tooMany (i, j) = tooMany (i + j)