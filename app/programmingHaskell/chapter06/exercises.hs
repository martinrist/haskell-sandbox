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



------------------------------
-- 6.14 - Chapter Exercises --
------------------------------


---------------------
-- Does it typecheck?
---------------------

-- Question 1

-- This doesn't typecheck (missing `Show`)
-- data Person = Person Bool
data Person = Person Bool deriving Show

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)


-- Question 2

-- This doesn't typecheck (missing `Eq`)
-- data Mood = Blah | Woot deriving Show
data Mood = Blah | Woot deriving (Eq, Show)

settleDown x = if x == Woot
                  then Blah
                  else x


-- Question 4

type Subject = String
type Verb = String
type Object = String

data Sentence =
    Sentence Subject Verb Object
    deriving (Eq, Show)

-- `s1` doesn't have an `Object` data constructor, so its type is `Object -> Sentence`
s1 = Sentence "dogs" "drool"

-- `s2` has all data constructors, so its type is `Sentence`
s2 = Sentence "Julie" "loves" "dogs"



------------------------------------------------
-- Given a datatype declaration, what can we do?
------------------------------------------------

data Rocks = Rocks String deriving (Eq, Show)
data Yeah = Yeah Bool deriving (Eq, Show)
data Papu = Papu Rocks Yeah deriving (Eq, Show)

-- Question 1
-- This won't typecheck, because "chases" is type `[Char]`, not `Rocks`
-- phew = Papu "chases" True

-- Question 2
-- This will typecheck correctly
truth = Papu (Rocks "chmoskydoz") (Yeah True)

-- Question 3
-- This will typecheck, because `Papu` derives `Eq` as do all of the types it uses
equalityForAll :: Papu -> Papu -> Bool
equalityForAll p p' = p == p'

-- Question 4
-- This won't typecheck because the types don't derive `Ord`
-- comparePapus :: Papu -> Papu -> Bool
-- comparePapus p p' = p > p'

data Rocks2 = Rocks2 String deriving (Eq, Show, Ord)
data Yeah2 = Yeah2 Bool deriving (Eq, Show, Ord)
data Papu2 = Papu2 Rocks2 Yeah2 deriving (Eq, Show, Ord)

comparePapus' :: Papu2 -> Papu2 -> Bool
comparePapus' p p' = p > p'



------------------
-- Match the types
------------------

-- Question 1
i :: Num a => a
--i :: a                    -- This doesn't work
i = 1

-- Questions 2 / 3 / 4
--f :: Float
--f :: Num a => a             -- This doens't work - 1.0 is `Fractional a =. a`
--f :: Fractional a => a      -- This will work with the `Fractional` constraint
f :: RealFrac a => a          -- This will work, since `instance RealFrac Float`
f = 1.0

-- Question 5
--freud :: a -> a
freud :: Ord a => a -> a      -- This works, but is overly restrictive
freud x = x

-- Question 6
--freud' :: a -> a
freud' :: Int -> Int          -- This works, but we can now only call `freud'` on an integer
freud' x = x

-- Questions 7 / 8
myX = 1 :: Int
-- sigmund :: Int -> Int
-- sigmund :: a -> a           -- This won't work - must return something of type `Int`
-- sigmund :: Num a => a -> a  -- This won't work, still not a concrete type
sigmund x = myX

-- Question 9
jung :: Ord a => [a] -> a
-- jung :: [Int] -> Int       -- This will work, but is overly restrictive
jung xs = head (sort xs)

-- Question 10
--young :: [Char] -> Char
young :: Ord a => [a] -> a      -- This works - `Ord` allows the use of `sort`
young xs = head (sort xs)

-- Question 11
mySort :: [Char] -> [Char]
mySort = sort

signifier :: [Char] -> Char
--signifier :: Ord a => [a] -> a   -- This won't work - `mySort` requires `a` to be `Char`
signifier xs = head (mySort xs)




--------------------------------------
-- Type-Kwon-Do Two: Electric Typealoo
--------------------------------------



chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f x y = (f x) == y


arith :: Num b => (a -> b) -> Integer -> a -> b
arith f i a = f a ^ i
















--chk f x y = (f x) == y

--arith f _ y = f y

