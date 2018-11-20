module ProgrammingHaskell.Chapter16.Exercises.HeavyLifting where

-- Exercise 1

a :: [Int]
a = fmap (+1) $ read "[1]"


-- Exercise 2

b :: Maybe [String]
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])


-- Exercise 3

c :: Num a => a -> a
c = fmap (*2) (\x -> x - 2)


-- Exercise 4

d :: Integer -> String
d = fmap (("1" ++) . show) (\x -> [x, 1..3])


-- Exercise 5

e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = fmap (read . ("123" ++) . show) ioi
    in fmap (*3) changed
