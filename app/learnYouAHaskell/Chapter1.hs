module Chapter1 where

------------------------------------------------
-- Chapter 1 - Starting Out                   --
-- (http://learnyouahaskell.com/starting-out) --
------------------------------------------------


-- Simple function definition (without a type signature)
-- Function names can't start with a capital letter
doubleMe x = x + x
--DoubleMe x = x + x                            -- Not in scope: data constructor 'DoubleMe'


-- Functions can use forward definitions
tripleUs x y = tripleMe x + tripleMe y
tripleMe x = x * 3


-- Conditionals are expressions, so they must have an `else` clause
doubleIfSmall x = if x > 100 then x else x * 2

-- > doubleIfSmall 100                  -- 200
-- > doubleIfSmall 101                  -- 101


-- Apostrophes are valid in identifiers, so we can use it to denote strict
-- (i.e. non-lazy) versions or modified versions of functions.
doubleIfSmall' x = (if x > 100 then x else x * 2) + 1

-- > doubleIfSmall' 100                 -- 201
-- > doubleIfSmall' 101                 -- 102

main = putStrLn



-----------
-- Lists --
-----------

-- Lists are homogeneous, surrounded by square brackets, separated by commas
numberList = [1, 2, 3, 4, 5]
stringList = ["One", "Two", "Three", "Four", "Five"]

-- brokenList = [1, "Two"]              -- Compile error


-- Strings are just lists of characters:
-- ['h', 'e', 'l', 'l', 'o']            -- "hello"


-- String concatenation uses the `++` operator, but Haskell walks through the
-- first list, so performance of a ++ b is O(length(a))
-- [1, 2, 3] ++ [4, 5, 6]               -- [1, 2, 3, 4, 5, 6]


-- Adding a new head to a list using the `:` (cons) operator is constant time
-- 0 : numberList                       -- [0, 1, 2, 3, 4, 5]


-- Access list elements by index using the `!!` operator:
-- Note that this is linear in the access length.
-- numberList !! 0                      -- 1
-- numberList !! 5                      -- *** Exception: Prelude.(!!): index too large


-- Length, head, tail, init and last:
-- `length` is also linear in performance.
-- length numberList                    -- 5
-- head numberList                      -- 1
-- head []                              -- *** Exception: Prelude.head: empty list
-- tail numberList                      -- [2, 3, 4, 5]
-- init numberList                      -- [1, 2, 3, 4]
-- last numberList                      -- 5


-- Testing for empty lists:
-- null numberList                      -- False
-- null []                              -- True


-- Reversing a list:
-- reverse numberList                   -- [5, 4, 3, 2, 1]


-- Taking and dropping elements from a list:
-- take 3 numberList                    -- [1, 2, 3]
-- drop 3 numberList                    -- [4, 5]


-- Maximum and minimum elements of a list:
-- maximum numberList                   -- 5
-- minimum numberList                   -- 1


-- Sum and product of elements:
-- sum numberList                       -- 15 (= 1 + 2 + 3 + 4 + 5)
-- product numberList                   -- 120 (= 5!)
-- sum stringList                       -- Error: can't apply sum to [Char]


-- Containment test (either as prefix or, more commonly, infix):
-- elem 3 numberList                    -- True
-- elem 6 numberList                    -- False
-- 3 `elem` numberList                  -- True


-- Functions to produce long or infinite lists:
-- cycle [1, 2, 3]                      -- [1, 2, 3, 1, 2, 3, 1, 2, 3 ....]
-- repeat 5                              -- [5, 5, 5, 5, 5, 5, 5, 5, 5 ....]
-- replicate 3 10                        -- [10, 10, 10]



------------
-- Ranges --
------------

-- Ranges are lists of elements that can be enumerated:
-- [1..5]                               -- [1, 2, 3, 4, 5]
-- ['a'..'e']                           -- "abcde"
-- [2, 4 .. 10]                         -- [2, 4, 6, 8, 10]
-- [5..1]                               -- []
-- [5, 4 .. 1]                          -- [5, 4, 3, 2, 1]
-- [13, 26 ..]                          -- [13, 26, 39, 52, 65 ...] - infinite list
-- take 5 [13, 26 ..]                   -- [13, 26, 39, 52, 65]



-------------------------
-- List Comprehensions --
-------------------------

-- Simple example of binding single element to contents of a range
-- [x * 2 | x <- [1..5]]                -- [2, 4, 6, 8, 10]


-- Adding a predicate to filter which items are taken from the range
-- [x * 2 | x <- [1..5], even x]        -- [4, 8]


-- Binding different variables to different lists
-- [x * y | x <- [1..3], y <- [1..3]]   -- [1, 2, 3, 2, 4, 6, 3, 6, 9]



------------
-- Tuples --
------------

-- Whereas lists are homogenous, tuples are heterogenous.  They have a fixed size:
-- :t (1, 3)                            -- (Num t1, Num t) => (t, t1)
-- :t (1, "foo")                        -- Num t => (t, [Char])


-- A tuple of size 2 (a 'pair') and a tuple of size 3 (a 'triple') are type-incompatible
-- [(1, 2), (1, 2, 3)]                  -- Error


-- Two pairs with different data types are also type-incompatible
-- [(1, 2), (3, "Foo")]                 -- Error


-- `fst` and `snd` are commonly applied to pairs, but don't work on tuples of larger size:
-- fst (1, 2)                           -- 1
-- snd (1, 2)                           -- 2


-- `zip` creates pairs by 'zipping' together two lists, that don't have to be the same size:
-- zip [1..5] "hello"                   -- [(1,'a'), (2,'b'), (3,'c'), (4,'d'), (5,'e')]
-- zip [1..2] "hello"                   -- [(1,'a'), (2,'b')]