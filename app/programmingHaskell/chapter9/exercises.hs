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