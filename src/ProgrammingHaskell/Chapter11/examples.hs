module ProgrammingHaskell.Chapter11.Examples where

------------------------------------
-- 11.3 - Data and type constructors
------------------------------------
-- Nullary constructors, or constants
data Trivial =
    Trivial'

-- Unary constructors
data UnaryTypeCon a =
    UnaryValueCon a

-- Binary constructors
data BinaryTypeCon a b =
    BinaryValueCon a
                   b

--------------------------------------
-- 11.5 - Data constructors and values
--------------------------------------
-- PugData is a constant value
data PugType =
    PugData

-- The type variable argument 'a' does not appear as an argument to the data constructor, so it is a 'phantom'
data HuskyType a =
    HuskyData

data Doggies a
    = Husky a
    | Mastiff a
    deriving (Eq, Show)

----------------------------------------
-- 11.6 - What's a type and what's data?
----------------------------------------
data Price =
    Price Integer
    deriving (Eq, Show)

data Manufacturer
    = Mini
    | Mazda
    | Tata
    deriving (Eq, Show)

data Airline
    = PapuAir
    | CatapultsR'Us
    | TakeYourChancesUnited
    deriving (Eq, Show)

data Vehicle
    = Car Manufacturer
          Price
    | Plane Airline

------------------------
-- 11.11 - Product Types
------------------------
-- This sum type has cardinality 3
data QuantumBool
    = QuantumTrue
    | QuantumFalse
    | QuantumBoth
    deriving (Eq, Show)

-- This product type has cardinality 9 (i.e. 3 * 3)
data TwoQs =
    MkTwoQs QuantumBool
            QuantumBool
    deriving (Eq, Show)

-- Person datatype without records requires manually-defined accessors
data Person =
    MkPerson String
             Int
    deriving (Eq, Show)

getName :: Person -> String
getName (MkPerson s _) = s

getAge :: Person -> Int
getAge (MkPerson _ i) = i

-- With records...
data PersonRecord = PersonRecord
    { name :: String
    , age :: Int
    } deriving (Eq, Show)

----------------------
-- 11.12 - Normal form
----------------------
-- Two simple datatypes, each with nullary constructors - cardinality = 1
data Fiction =
    Fiction
    deriving (Show)

data Nonfiction =
    Nonfiction
    deriving (Show)

-- A sum type, with cardinality 2
data BookType
    = FictionBook Fiction
    | NonfictionBook Nonfiction
    deriving (Show)

-- A simple type synonym - infinite cardinality
type AuthorName = String

-- Product type 
data Author =
    Author (AuthorName, BookType)

-----------------------
-- 11.17 - Binary trees
-----------------------
data BinaryTree a
    = Leaf
    | Node (BinaryTree a)
           a
           (BinaryTree a)
    deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
    | b == a = Node left a right
    | b < a = Node (insert' b left) a right
    | b > a = Node left a (insert' b right)
