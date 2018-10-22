{-# LANGUAGE DeriveGeneric #-}
module ProgrammingHaskell.Chapter14.Examples where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Gen (oneof)
import GHC.Generics

main :: IO ()
main = hspec $ do
    describe "Addition" $ do
        it "15 divided by 3 is 5" $ do
            dividedBy 15 3 `shouldBe` (5, 0)
        it "22 divided by 5 is 4 remainder 2" $ do
            dividedBy 22 5 `shouldBe` (4, 2)
        it "x + 1 is always greater than x" $ do
            property $ \x -> x + 1 > (x :: Int)

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
    where go n d count
           | n < d = (count, n)
           | otherwise = go (n - d) d (count + 1)

trivialInt :: Gen Int
trivialInt = return 1

oneThroughThree :: Gen Int
oneThroughThree = elements [1, 2, 3]

oneThroughTen :: Gen Int
oneThroughTen = choose (1, 10)

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
    a <- arbitrary
    b <- arbitrary
    return (a, b)

genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
    a <- arbitrary
    frequency [ (1, return Nothing),
                (3, return (Just a))]


prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater


-----------------------------------
-- 14.6 - Kicking around QuickCheck
-----------------------------------

-- Arbitrary instance for a trivial type
data Trivial =
    Trivial
    deriving (Eq, Show)

trivialGen :: Gen Trivial
trivialGen = return Trivial

instance Arbitrary Trivial where
    arbitrary = trivialGen

-- Arbitrary instance for an identity type
data Identity a =
    Identity a
    deriving (Eq, Show)

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
    a <- arbitrary
    return (Identity a)

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = identityGen

-- Arbitrary instance for a simple product type
data Pair a b =
    Pair a b
    deriving (Eq, Show)

pairGen :: (Arbitrary a, Arbitrary b) => Gen (Pair a b)
pairGen = do
    a <- arbitrary
    b <- arbitrary
    return (Pair a b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
    arbitrary = pairGen

-- Arbitrary instance for a sum type
data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

sumGenEqual :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
sumGenEqual = do
    a <- arbitrary
    b <- arbitrary
    oneof [return $ First a,
           return $ Second b]

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
    arbitrary = sumGenEqual

sumGenMoreFirst :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
sumGenMoreFirst = do
    a <- arbitrary
    b <- arbitrary
    frequency [(10, return $ First a),
               ( 1, return $ Second b)]



-- Coarbitrary

data Bool' =
    True'
  | False'
  deriving (Generic)

instance CoArbitrary Bool'

trueGen :: Gen Int
trueGen = coarbitrary True' arbitrary

falseGen :: Gen Int
falseGen = coarbitrary False' arbitrary


arbFunc :: Gen (Bool' -> Int)
arbFunc = arbitrary
