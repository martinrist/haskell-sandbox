---------------
-- Exercises --
---------------
{-# LANGUAGE FlexibleInstances #-}

import Data.Char

-- Exercises: Vehicles
----------------------

data Price = Price Integer deriving (Eq, Show)

data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)

data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited deriving (Eq, Show)

data Vehicle = Car Manufacturer Price
             | Plane Airline

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir


-- Question 2
isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar


-- Question 3
-- This is a partial function - caling `getManu doge` throws an exception
getManu :: Vehicle -> Manufacturer
getManu (Car x _) = x


-- Question 5
data Size = Size Integer
data Vehicle' = Car' Manufacturer Price
              | Plane' Airline Size

isCar' :: Vehicle' -> Bool
isCar' (Car' _ _) = True
isCar' _ = False

isPlane' :: Vehicle' -> Bool
isPlane' (Plane' _ _) = True
isPlane' _ = False


-- Exercises: Logic Goats
-------------------------

class TooMany a where
    tooMany :: a -> Bool

instance TooMany Int where
    tooMany n = n > 42

instance TooMany Float where
    tooMany n = n > 42

-- Question 1
instance TooMany (Int, String) where
    tooMany (i, _) = tooMany i

-- Question 2
instance TooMany (Int, Int) where
    tooMany (i, j) = tooMany (i + j)

-- Question 3
instance (Num a, TooMany a) => TooMany (a, a) where
    tooMany (i, j) = tooMany (i + j)




-- 11.17 - Binary Tree
----------------------

-- Write `map` for binary tree

data BinaryTree a = 
        Leaf
      | Node (BinaryTree a) a (BinaryTree a)
      deriving (Eq, Ord, Show)


mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
    Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' =
    Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected =
    Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

mapOkay =
    if mapTree (+1) testTree' == mapExpected
    then print "Passed"
    else error "Failed"



-- Convert binary trees to lists

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = a : [] ++ preorder left ++ preorder right

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = inorder left ++ a : [] ++ inorder right

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = postorder left ++ postorder right ++ a : []

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
    if preorder testTree == [2, 1, 3]
       then putStrLn "Preorder fine!"
       else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
    if inorder testTree == [1, 2, 3]
    then putStrLn "Inorder fine!"
    else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder =
    if postorder testTree == [1, 3, 2]
    then putStrLn "Postorder fine!"
    else putStrLn "postorder failed check"

main :: IO ()
main = do
    testPreorder
    testInorder
    testPostorder


-- foldr for BinaryTree
foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f z = foldr f z . inorder



-- Chapter Exercises: As-patterns
---------------------------------

-- Question 1
isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf xall@(x:xs) (y:ys)
    | x == y    = isSubsequenceOf xs ys
    | otherwise = isSubsequenceOf xall ys


-- Question 2
capitaliseWords :: String -> [(String, String)]
capitaliseWords ws = [(w, capitaliseWord w) | w <- words ws]


-- Chapter Exercises: Language Exercises
----------------------------------------

-- Question 1

capitaliseWord :: String -> String
capitaliseWord "" = ""
capitaliseWord (x:xs) = toUpper x : xs


