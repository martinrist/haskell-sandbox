module ProgrammingHaskell.Chapter14.Exercises.UsingQuickCheckSpec where

import Test.Hspec
import Test.QuickCheck

half :: Fractional a => a -> a
half x = x / 2

halfIdentity :: Fractional a => a -> a
halfIdentity = (*2) . half

prop_halfIdentity :: (Eq a, Fractional a) => a -> Bool
prop_halfIdentity x = x == halfIdentity x

testHalfIdentity :: Spec
testHalfIdentity =
    context "half" $
        it "halfIdentity property holds" $
            property (prop_halfIdentity :: Double -> Bool)

spec :: Spec
spec = do
    testHalfIdentity
