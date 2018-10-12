module ProgrammingHaskell.Chapter12.Exercises.Natural where

data Nat
    = Zero
    | Succ Nat
    deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + natToInteger n

integerToNat :: Integer -> Maybe Nat
integerToNat x
    | x < 0 = Nothing
    | otherwise = Just $ go x
  where
    go 0 = Zero
    go x = Succ $ go (x - 1)