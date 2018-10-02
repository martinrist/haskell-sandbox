{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- Chapter 4 - Notes
module ProgrammingHaskell.Chapter04.Examples where

import Data.Tuple

-- `Mood` is the type constructor
-- `Blah` and `Woot` are data constructors
-- `deriving Show` just gives us the ability to print results
data Mood
    = Blah
    | Woot
    deriving (Show)

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood Woot = Blah

-- `Int`s are fixed precision, `Integer` is arbitrary
smallInt = 42 :: Int

hugeInteger = 474738229127373717912172 :: Integer

-- `Float` and `Double` are fixed precision
smallFloat = 42.42 :: Float

-- note that this doesn't error, it just returns `Infinity`
tooLargeForFloat = 1e40 :: Float

notTooLargeForDouble = 1e40 :: Double

-- `Rational` represents fractions
oneHalf = 1 / 2 :: Rational

twoThirds = 6 / 9 :: Rational

-- If / then / else example
greetIfCool :: String -> IO ()
greetIfCool coolness =
    if cool
        then putStrLn "eyyy. What's shakin'?"
        else putStrLn "pshhhhh."
  where
    cool = coolness == "downright frosty yo"

-- ====================
-- Section 4.5 - Tuples
-- ====================
-- `fst` and `snd` extract values from a two-tuple
myTup = (1, "blah")

first = fst myTup

second = snd myTup

-- `swap` is imported from `Data.Tuple`
swapped = swap myTup

-- We can define n-tuples, but can't call these funtions on them
-- oops = swap (1, 2, 3)
-- alsoOops = fst (1, 2, 3)
-- ===================
-- Section 4.6 - Lists
-- ===================
-- This is of type `[[Char]]` or equivalently, `[String]`
awesome = ["Papuchon", "curry", "Haskell"]

alsoAwesome = ["Quake", "The Simons"]

-- We can combine lists with ++
bothAwesome = awesome ++ alsoAwesome

allAwesome = [awesome, alsoAwesome]

-- ===============================
-- Section 4.7 - Chapter Exercises
-- ===============================
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x =
    let reversed = reverse x
     in x == reversed

myAbs :: Integer -> Integer
myAbs x =
    if x < 0
        then negate x
        else x

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f x y = ((snd x, snd y), (fst x, fst y))