module LearnYouAHaskell.Chapter2 where

---------------------------------------------------------
-- Chapter 2 - Types and Typeclasses                   --
-- (http://learnyouahaskell.com/types-and-typeclasses) --
---------------------------------------------------------

-------------------------------
-- Explicit Type Declaration --
-------------------------------

-- In GHCi, prefix `:t` to an expression to show the type
    -- :t (True, 'a')                       -- (True, 'a') :: (Bool, Char)


-- A 1-arity function with an explicit type signature
-- `[Char]` is equivalent to `String`
--removeNonUppercase :: [Char] -> [Char]
removeNonUppercase :: String -> String
removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]


-- A 2-arity function lists has a type signature like the following:
addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z


-- Because `->` is right-associative, the type signature is:
addThree' :: Int -> (Int -> (Int -> Int))
addThree' x y z = x + y + z
-- i.e. `addThree'` is a function that takes an `Int` and returns
--                          a function that takes an `Int` and returns
--                              a function that takes an `Int` and returns an `Int`



--------------------------
-- Common Haskell Types --
--------------------------

-- Int          -- Bounded integer value (usually -2^63 - 2^63)
-- Integer      -- Unbounded integer vlaue, less efficient than `Int`
-- Float        -- Real single-precision floating point number
-- Double       -- Real double-precision flouting point number
-- Bool         -- Boolean value, either `True` or `False`
-- Char         -- Unicode character, literal surrounded by single quotes



--------------------
-- Type Variables --
--------------------

-- Type variables allow functions to operate on values of various types in a
-- type-safe manner (like generics).
-- :t head                              -- head :: [a] -> a
-- :t fst                               -- fst :: (a, b) -> a



------------------
-- Type Classes --
------------------

-- Type classes are interfaces that define some behaviour.  If a type `t` is an
-- instance of a type class `c`, `t` supports and implements the behaviour
-- described by `c`.

--  `tc` specifies functinos.  When we make a type an instance of `tc` we define
-- what those functions mean for that type.

-- Look at type signature for `==` operator:
-- :t (==)                              -- (==) :: Eq a => a -> a -> Bool
-- Note: `==` is a function.  As it consists only of special characters, we
-- have to surround it with parens to tell the parser we mean the function.

-- `Eq a` before `=>` is a class constraint.  The type signature says that
-- `==` takes two values of type `a`, provided `a` is a type that is an
-- instance of `Eq`.



-------------------------
-- The `Eq` Type Class --
-------------------------

-- `Eq` defines `==` and `\=`:
-- 5 /= 5                               -- False
-- "Ho Ho" == "Ho Ho"                   -- True



--------------------------
-- The `Ord` Type Class --
--------------------------

-- `Ord` is a type class for types whose values permit a total ordering.
-- Functions like `>` operate on parameters whose types conform to `Ord`:
-- :t (>)                               -- (>) :: Ord a => a -> a -> Bool

-- `Ord` defines the following functions, and extends `Eq`:
    -- (<) :: Ord a => a -> a -> Bool
    -- (<=) :: Ord a => a -> a -> Bool
    -- (>) :: Ord a => a -> a -> Bool
    -- (>=) :: Ord a => a -> a -> Bool
    -- max :: Ord a => a -> a -> a
    -- min :: Ord a => a -> a -> a
    -- compare :: Ord a => a -> a -> Ordering

-- `Ordering` is a type that can be `GT`, `LT` or `EQ`:
-- compare 1 2                          ; LT
-- compare 2 1                          ; GT



---------------------------
-- The `Show` Type Class --
---------------------------

-- Values whose types are instances of the `Show` type can be represented as
-- strings, via calling the `show` function:
-- :t show                              -- show :: Show a => a -> String
-- show 3                               -- "3"
-- show 5.334                           -- "5.334"
-- show True                            -- "True"



---------------------------
-- The `Read` Type Class --
---------------------------

-- `Read` is the opposite of `Show`.  `read` function takes a string and returns
-- a value whose type is an instance of `Read`:
-- :t read                              -- read :: Read a => String -> a
-- read "True" || False                 -- True
-- read "5" - 2                         -- 3


-- If we just call `read "4"`, Haskell doesn't know which type to parse to.
-- We need to give a type annotation:
-- read "4"                             -- *** Exception: Prelude.read: no parse
-- read "4" :: Int                      -- 4
-- read "(3, 4.0)" :: (Int, Float)      -- (3, 4.0)



---------------------------
-- The `Enum` Type Class --
---------------------------

-- `Enum` instances are sequentially ordered types whose values can be enumerated.
-- e.g. `Bool`, `Char`, `Int`.  They can be used in range literals.
-- ['a' .. 'e']                         -- "abcde"

-- Main methods are `succ` and `pred`:
-- succ 8                               -- 9
-- pred 'b'                             -- 'a'



------------------------------
-- The `Bounded` Type Class --
------------------------------

-- `Bounded` instances have an upper and lower bound, which can be checked
-- using `minBound` and `maxBound`:

-- minBound :: Int                      -- -9223372036854775808
-- maxBound :: Bool                     -- True

-- `minBound` and `maxBound` have a type signature of `Bounded a => a`, so
-- they are polymorphic constants.

-- Tuples whose components are all instances of `Bounded` are themselves
-- bounded:
-- maxBound :: (Bool, Int)              -- (True, 9223372036854775807)
-- maxBound :: (Bool, String)           -- Error - String is not `Bounded`



--------------------------
-- The `Num` Type Class --
--------------------------

-- Instances of `Num` are types that can act like numbers.  Many mathematical
-- functions operate on types which are instances of `Num`:
-- :t (*)                               -- (*) :: Num a => a -> a -> a

-- The two arguments must be the same type, so:
-- (5 :: Int) * (6 :: Integer)          -- Error - `Int` and `Integer` are incompatible
-- 5 * (6 :: Integer)                   -- 30 - here 5 acts as an `Integer`

-- To be an instance of `Num`, a type must already be in `Show` and `Eq`.



-------------------------------
-- The `Floating` Type Class --
-------------------------------

-- The `Floating` type class includes the `Float` and `Double` types.
-- :t sin                               -- sin :: Floating a => a -> a



-------------------------------
-- The `Integral` Type Class --
-------------------------------

-- The `Integral` type class includes only integral numbers.  Covers
-- `Int` and `Integer` types:
-- :t mod                               -- mod :: Integral a => a -> a -> a

-- `fromIntegral` converts an `Integral` into another type:
-- :t fromIntegral                      -- fromIntegral :: (Num b, Integral a) => a -> b
-- fromIntegral 1 :: Float              -- 1.0
