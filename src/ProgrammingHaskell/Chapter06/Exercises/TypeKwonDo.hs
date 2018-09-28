module ProgrammingHaskell.Chapter06.Exercises.TypeKwonDo where

-- Question 1
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f a b = f a == b

-- Question 2
arith :: Num b => (a -> b) -> Integer -> a -> b
arith f i a = f a ^ i