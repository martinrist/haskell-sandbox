------------------------
-- Chapter 9 - Exercises
------------------------

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
