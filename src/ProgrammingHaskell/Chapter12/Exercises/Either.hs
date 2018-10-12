module ProgrammingHaskell.Chapter12.Exercises.Either where

import Control.Applicative

-- Question 1
lefts' :: [Either a b] -> [a]
lefts' = foldr acc []
  where
    acc (Left a) as = a : as
    acc (Right _) as = as

-- Question 2
rights' :: [Either a b] -> [b]
rights' = foldr acc []
  where
    acc (Left _) bs = bs
    acc (Right b) bs = b : bs

-- Question 3
partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = liftA2 (,) lefts' rights'

-- Question 4
eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right b) = Just $ f b

-- Question 5
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a) = f a
either' _ g (Right b) = g b

-- Question 6
eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' g = either' (const Nothing) (Just . g)