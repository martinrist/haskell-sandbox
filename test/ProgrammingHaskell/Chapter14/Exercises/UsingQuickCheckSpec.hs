module ProgrammingHaskell.Chapter14.Exercises.UsingQuickCheckSpec where

import           Test.Hspec
import           Test.QuickCheck
import           Data.List                      ( sort )

-- Question 1 - half

half :: Fractional a => a -> a
half x = x / 2

halfIdentity :: Fractional a => a -> a
halfIdentity = (* 2) . half

prop_halfIdentity :: (Eq a, Fractional a) => a -> Bool
prop_halfIdentity x = x == halfIdentity x

testHalfIdentity :: Spec
testHalfIdentity = context "half" $ it "halfIdentity property holds" $ property
    (prop_halfIdentity :: Double -> Bool)

-- Question 2 - listOrdered

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
  where
    go _ status@(_      , False) = status
    go y (       Nothing, t    ) = (Just y, t)
    go y (       Just x , t    ) = (Just y, x >= y)

prop_listOrdered :: (Ord a) => [a] -> Bool
prop_listOrdered = listOrdered . sort

testListOrdered :: Spec
testListOrdered =
    context "listOrdered" $ it "listOrdered property holds" $ property
        (prop_listOrdered :: [Int] -> Bool)

-- Question 3 - plus

plusAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
plusAssociative x y z = x + (y + z) == (x + y) + z

plusCommutative :: (Eq a, Num a) => a -> a -> Bool
plusCommutative x y = x + y == y + x

testPlus :: Spec
testPlus = context "plus" $ do
    it "Associative property holds for plus"
        $ property (plusAssociative :: Int -> Int -> Int -> Bool)
    it "Commutative property holds for plus"
        $ property (plusCommutative :: Int -> Int -> Bool)

-- Question 4 - multiply

multiplyAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
multiplyAssociative x y z = x * (y * z) == (x * y) * z

multiplyCommutative :: (Eq a, Num a) => a -> a -> Bool
multiplyCommutative x y = x * y == y * x

testMultiply :: Spec
testMultiply = context "multiply" $ do
    it "Associative property holds for multiply"
        $ property (multiplyAssociative :: Int -> Int -> Int -> Bool)
    it "Commutative property holds for multiply"
        $ property (multiplyCommutative :: Int -> Int -> Bool)

-- Question 5 - quot / rem and div / mod

quotRemProperty :: (Eq a, Integral a) => a -> a -> Bool
quotRemProperty _ 0 = True
quotRemProperty x y = quot x y * y + rem x y == x

divModProperty :: (Eq a, Integral a) => a -> a -> Bool
divModProperty _ 0 = True
divModProperty x y = div x y * y + mod x y == x

testQuotRemDivMod :: Spec
testQuotRemDivMod = context "quotRemDivMod" $ do
    context "quotRem" $ it "quotRem property holds" $ property
        (quotRemProperty :: Int -> Int -> Bool)
    context "divMod" $ it "divMod propoerty holds" $ property
        (divModProperty :: Int -> Int -> Bool)

-- Question 7 - reverse twice

reverseTwiceProperty :: Eq a => [a] -> Bool
reverseTwiceProperty xs = (reverse . reverse) xs == xs

testReverseTwice :: Spec
testReverseTwice = context "reverse" $ do
    it "reverse twice is the same as id for [Int]" $
        property (reverseTwiceProperty :: [Int] -> Bool)
    it "reverse twice is the same as id for [Char]" $
        property (reverseTwiceProperty :: String -> Bool)

-- Question 8 - $ and composition

applicationProperty :: Eq b => Fun a b -> a -> Bool
applicationProperty (Fun _ f) a = (f $ a) == f a

testApplicationProperty :: Spec
testApplicationProperty = context "Function application" $
    it "`f $ a` gives the same result as `f a`" $
        property (applicationProperty :: Fun Int Int -> Int -> Bool)

compositionProperty :: Eq c => Fun a b -> Fun b c -> a -> Bool
compositionProperty (Fun _ f) (Fun _ g) a = (g . f) a == g(f a)

testCompositionProperty = context "Function composition" $
    it "`(f . g) a` gives the same result as f (g a)`" $
        property (compositionProperty :: Fun Int String -> Fun String Int -> Int -> Bool)

-- Question 9 - foldr

foldrColonProperty :: Eq a => [a] -> [a] -> Bool
foldrColonProperty xs ta = foldr (:) xs ta == xs ++ ta

testFoldrColonProperty :: Spec
testFoldrColonProperty = context "foldr (:)" $
    it "`foldr (:) == (++)`" $
       property (foldrColonProperty :: String -> String -> Bool)

foldrConcatProperty :: (Eq a, Foldable t) => t [a] -> Bool
foldrConcatProperty tas = foldr (++) [] tas == concat tas

testFoldrConcatProperty :: Spec
testFoldrConcatProperty = context "foldr (++) []" $
    it "`foldr (++) [] == concat`" $
        property (foldrConcatProperty :: [[Int]] -> Bool)

-- Question 10 - length (take n xs)

-- This property is false if n > length xs
lengthOfTakeProperty :: Int -> [a] -> Bool
lengthOfTakeProperty n xs = length (take n xs) == n

testLengthOfTakeProperty :: Spec
testLengthOfTakeProperty = context "length (take n xs)" $
    it "`length (take n xs) == n`" $
        property (lengthOfTakeProperty :: Int -> String -> Bool)

-- Question 11 - read . show

readShowProperty :: (Eq a, Show a, Read a) => a -> Bool
readShowProperty x = read (show x) == x

testReadShowProperty :: Spec
testReadShowProperty = context "read and show)" $
    it "`read . show == id`" $
        property (readShowProperty :: Int -> Bool)

spec :: Spec
spec = do
    testHalfIdentity
    testListOrdered
    testPlus
    testMultiply
    testQuotRemDivMod
    testApplicationProperty
    testCompositionProperty
    --  testFoldrColonProperty          -- Don't test this - it fails
    testFoldrConcatProperty
    -- testLengthOfTakeProperty         -- Don't test this, it's false for some values
    testReadShowProperty
