module RealWorld.Chapter03.GrahamScanSpec where

import Test.Hspec
import RealWorld.Chapter03.GrahamScan

testGoldMaster :: SpecWith ()
testGoldMaster = describe "Gold master tests for convex hull" $ do
    it "calculates correct convex hull for test polygon 1" $
        convexHull polygon1 `shouldBe` [Point 0 0, Point 3 1, Point 3 3, Point 1 4, Point (-1) 2, Point (-2) 1]
    it "calculates correct convex hull for test polygon 2" $
        convexHull polygon2 `shouldBe` [Point 0 0, Point 3 1, Point 2 4, Point 1 4]
    it "calculates correct convex hull for test polygon 3" $
        convexHull polygon3 `shouldBe` [Point 1 1, Point 4 1, Point 5 3, Point 1 4]
    where
        polygon1 = [Point 0 0, Point 3 1, Point 1 1, Point 3 3, Point 1 4, Point (-1) 2, Point (-2) 1]
        polygon2 = [Point 0 0, Point 3 1, Point 2 2, Point 1.75 2, Point 2 4, Point 1 4]
        polygon3 = [Point 1 1, Point 4 1, Point 5 3, Point 3 2, Point 2 3, Point 1 4]

testPointComparisons :: SpecWith()
testPointComparisons = describe "Test implementation of `Ord` for `Point`" $ do
    it "returns `EQ` when comparing a point to itself" $
        compare (Point 1 2) (Point 1 2) `shouldBe` EQ
    it "returns `LT` when `p1` is below-left `p2`" $
        compare (Point 1 1) (Point 2 2) `shouldBe` LT
    it "returns `LT` when `p1` is directly below `p2`" $
        compare (Point 1 1) (Point 1 2) `shouldBe` LT
    it "returns `LT` when `p1` is below-right `p2`" $
        compare (Point 2 1) (Point 1 2) `shouldBe` LT
    it "returns `LT` when `p1` is directly left of `p2`" $
        compare (Point 1 1) (Point 2 1) `shouldBe` LT
    it "returns `GT` when `p1` is directly right of `p2`" $
        compare (Point 2 1) (Point 1 1) `shouldBe` GT
    it "returns `GT` when `p1` is above-left `p2`" $
        compare (Point 2 2) (Point 3 1) `shouldBe` GT
    it "returns `GT` when `p1` is directly above `p2`" $
        compare (Point 2 2) (Point 2 1) `shouldBe` GT
    it "returns `GT` when `p1` is above-right `p2`" $
        compare (Point 2 2) (Point 1 1) `shouldBe` GT

testTurnDirection :: SpecWith()
testTurnDirection = describe "Test implementation of `turnDirection`" $ do
    it "returns `Straight` for three identical points" $
        turnDirection (Point 1 1, Point 1 1, Point 1 1) `shouldBe` Straight
    it "returns `Straight` for two identical points" $
        turnDirection (Point 1 1, Point 1 1, Point 2 3) `shouldBe` Straight
    it "returns `Straight` for three collinear points" $
        turnDirection (Point 1 1, Point 2 2, Point 3 3) `shouldBe` Straight
    it "returns `CounterClockwise` for simple counter-clockwise right-angled turn" $
        turnDirection (Point 1 1, Point 2 1, Point 2 2) `shouldBe` CounterClockwise
    it "returns `Clockwise` for simple clockwise right-angled turn" $
        turnDirection (Point 2 2, Point 2 1, Point 1 1) `shouldBe` Clockwise
    it "correctly calculates counter-clockwise turn for points in different quadrants" $
        turnDirection (Point 1 1, Point (-1) 1, Point (-1) (-1)) `shouldBe` CounterClockwise
    it "correctly calculates clockwise turn for points in different quadrants" $
        turnDirection (Point (-1) (-1), Point (-1) 1, Point 1 1) `shouldBe` Clockwise

testVectorComparisons :: SpecWith()
testVectorComparisons = describe "Test implementation of `Ord` for `Vector`" $
    it "Tests" $
        pendingWith "Tests not implemented yet"

spec :: Spec
spec = do
    testGoldMaster
    testPointComparisons
    testTurnDirection
    testVectorComparisons
