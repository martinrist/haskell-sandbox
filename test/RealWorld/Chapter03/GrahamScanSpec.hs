module RealWorld.Chapter03.GrahamScanSpec where

import Test.Hspec
import RealWorld.Chapter03.GrahamScan

test1 = describe "Test 1" $
    it "Direction Equality" $
        (CounterClockwise == CounterClockwise) `shouldBe` True

spec :: Spec
spec = do
    describe "Basic test" $ do test1
