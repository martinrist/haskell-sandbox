import Data.List (sort)

------------------
-- Eq instances --
------------------

-- Question 1
data TisAnInteger = TisAn Integer
instance Eq TisAnInteger where
    (==) (TisAn i) (TisAn j) = i == j

-- Question 2
data TwoIntegers = Two Integer Integer
instance Eq TwoIntegers where
    (==) (Two a b) (Two a' b') = a == a' && b == b'

-- Question 3
data StringOrInt = TisAnInt Int | TisAString String
instance Eq StringOrInt where
    (==) (TisAnInt i) (TisAnInt j) = i == j
    (==) (TisAString s) (TisAString t) = s == t
    (==) _ _ = False

-- Question 4
data Pair a = Pair a a
instance Eq a => Eq (Pair a) where
    (==) (Pair x y) (Pair x' y') = x == x' && y == y'

-- Question 5
data Tuple a b = Tuple a b
instance (Eq a, Eq b) => Eq (Tuple a b) where
    (==) (Tuple x y) (Tuple x' y') = x == x' && y == y'

-- Question 6
data Which a = ThisOne a | ThatOne a
instance Eq a => Eq (Which a) where
    (==) (ThisOne a) (ThisOne a') = a == a'
    (==) (ThatOne a) (ThatOne a') = a == a'
    (==) _ _ = False

-- Question 7
data EitherOr a b = Hello a | Goodbye b
instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (==) (Hello a) (Hello a') = a == a'
    (==) (Goodbye b) (Goodbye b') = b == b'
    (==) _ _ = False



--------------------
-- Chapter Exercises
--------------------

-- Does it typecheck?

-- Question 1
data Person = Person Bool deriving Show

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)


-- Question 2
data Mood = Blah | Woot deriving (Show, Eq)
settleDown :: Mood -> Mood
settleDown x = if x == Woot
                  then Blah
                  else x

-- Question 3
type Subject = String
type Verb = String
type Object = String

data Sentence =
   Sentence Subject Verb Object
   deriving (Eq, Show)

-- This typechecks, but its type is Object -> Sentence
-- so we can't 'show' it.
s1 = Sentence "dogs" "drool"

s2 = Sentence "Julie" "loves" "dogs"


-- Given a datatype declaration, what can we do?

data Rocks = Rocks String deriving (Eq, Show)
data Yeah = Yeah Bool deriving (Eq, Show)
data Papu = Papu Rocks Yeah deriving (Eq, Show)

-- Question 1
--phew = Papu "chases" True
phew = Papu (Rocks "chases") (Yeah True)

-- Question 2
truth = Papu (Rocks "chomskydoz") (Yeah True)

-- Question 3
equalityForAll :: Papu -> Papu -> Bool
equalityForAll p p' = p == p'

-- Question 4
-- `Papu` isn't an instance of `Ord`
-- comparePapus :: Papu -> Papu -> Bool
-- comparePapus p p' = p > p'



-- Match the types

-- Question 1
i :: Num a => a
--i :: a               -- Can't substitute this
i = 1

-- Question 2
f :: Float
-- f :: Num a => a     -- Can't substitute this - needs to be Fractional
f = 1.0

-- Question 3
-- f' :: Float
f' :: Fractional a => a     -- This is fine
f' = 1.0

-- Question 4
--f'' :: Float
f'' :: RealFrac a => a      -- This is fine - RealFrac is a subclass of Fractional
f'' = 1.0

-- Question 5
--freud :: a -> a
freud :: Ord a => a -> a    -- This is fine, but unnecessary
freud x = x

-- Question 6
--freud' :: a -> a
freud' :: Int -> Int        -- This is fine, but overly concrete
freud' x = x

-- Question 7
myX = 1 :: Int
sigmund :: Int -> Int
--sigmund :: a -> a           -- Won't work because of the type of myX
sigmund x = myX

-- Question 8
sigmund' :: Int -> Int
--sigmund' :: Num a => a -> a -- Still won't work because of myX
sigmund' x = myX

-- Question 9
--jung :: Ord a => [a] -> a
jung :: [Int] -> Int        -- This is fine, but concrete
jung xs = head (sort xs)

-- Question 10
--young :: [Char] -> Char
young :: Ord a => [a] -> a  -- This is fine - more general
young xs = head (sort xs)

-- Question 11
mySort :: [Char] -> [Char]
mySort = sort
signifier :: [Char] -> Char
--signifier :: Ord a => [a] -> a  -- Won't work - mySort is [Char]
signifier xs = head (mySort xs)




-- Type-Kwon-Do

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f x y = (f x) == y

arith :: Num b => (a -> b) -> Integer -> a -> b
arith f _ y = f y

