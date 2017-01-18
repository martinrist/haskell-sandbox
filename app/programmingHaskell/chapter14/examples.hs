module Addition where

import Test.Hspec
import Test.QuickCheck

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