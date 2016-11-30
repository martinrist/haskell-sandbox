------------------------
-- Chapter 9 - Exercises
------------------------
import Data.Char

-- Exercise : EnumFromTo

eftBool :: Bool -> Bool -> [Bool]
eftBool f t
    | f == t        = [f]
    | f > t         = []
    | otherwise     = f:[] ++ (eftBool (succ f) t)

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd f t
    | f == t        = [f]
    | f > t         = []
    | otherwise     = f:[] ++ (eftOrd (succ f) t)

eftInt :: Int -> Int -> [Int]
eftInt f t
    | f == t        = [f]
    | f > t         = []
    | otherwise     = f:[] ++ (eftInt (succ f) t)

eftChar :: Char -> Char -> [Char]
eftChar f t
    | f == t        = [f]
    | f > t         = []
    | otherwise     = f:[] ++ (eftChar (succ f) t)



-- Exercises : Thy Fearful Symmetry
-----------------------------------

-- Question 1
myWords :: String -> [String]
myWords s
         | s == ""       = []
         | otherwise     = (takeWhile notBlank s) : [] ++ (myWords (dropWhile blank $ dropWhile notBlank s))
         where notBlank  = (/= ' ')
               blank     = (== ' ')

-- Question 2

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen


tokenise :: String -> Char -> [String]
tokenise s c
         | s == ""       = []
         | otherwise     = (takeWhile notMatch s) : [] ++ (tokenise (dropWhile match $ dropWhile notMatch s) c)
         where notMatch  = (/= c)
               match     = (== c)

myLines :: String -> [String]
myLines s = tokenise s '\n'



-- Exercises: Square Cube
-------------------------

mySqr = [ x^2 | x <- [1..5]]
myCube = [ x^3 | x <- [1..5]]

mySquaresAndCubes = [(x, y) | x <- mySqr, y <- myCube]

mySquaresAndCubesUnder50 = [(x, y) | x <- mySqr, x < 50, y <- myCube, y < 50]

countOfMySquaresAndCubesUnder50 = length mySquaresAndCubesUnder50


-- Exercises: Filtering
-----------------------

-- Question 1
multiplesOf3 = filter (\x -> (x `mod` 3) == 0)

-- Question 2
howManyMultiples = length . multiplesOf3

-- Question 3
myFilter :: String -> [String]
myFilter = (filter (\w -> not (elem w ["the", "a", "an"]))) . words


-- Exercises: Zipping
---------------------

-- Question 1
zip' :: [a] -> [b] -> [(a,b)]
zip' _ []          = []
zip' [] _          = []
zip' (x:xs) (y:ys) = (x, y) : (zip' xs ys)

-- Question 2
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ _ []          = []
zipWith' _ [] _          = []
zipWith' f (x:xs) (y:ys) = f x y : (zipWith' f xs ys)

-- Question 3
zip'' :: [a] -> [b] -> [(a, b)]
zip'' = zipWith' (\x y -> (x, y))


-- Chapter Exercises
--------------------

-- Question 2
filterUpper = filter isUpper

-- Question 3
capitaliseFirst :: String -> String
capitaliseFirst []     = []
capitaliseFirst (x:xs) = (toUpper x) : xs

-- Question 4
capitalise :: String -> String
capitalise []     = []
capitalise (x:xs) = (toUpper x) : (capitalise xs)

-- Questions 5 & 6
getFirstAsCapital = toUpper . head
