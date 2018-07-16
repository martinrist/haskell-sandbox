module RealWorld.Chapter03.GrahamScanSpec where

import Test.Hspec
import Test.QuickCheck
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

testPointComparisons :: SpecWith ()
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

testTurnDirection :: SpecWith ()
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

testVectorComparisons :: SpecWith ()
testVectorComparisons = describe "Test implementation of `Ord` for `Vector`" $
    it "Tests" $
        pendingWith "Tests not implemented yet"



testToTriplesProperties :: SpecWith()
testToTriplesProperties = describe "Test properties of `toTriples`" $
    it "returns a list with the same number of elements" $ property $
        \xs -> length (xs :: [Point]) == length (toTriples xs)




testToTriples :: SpecWith ()
testToTriples = describe "Test implementation of `toTriples`" $ do
    it "returns empty list when called on empty list" $
        toTriples ([] :: [()]) `shouldBe` []
    it "returns single triple when called on list with single item" $
        toTriples [()]  `shouldBe` [((), (), ())]
    it "returns two triples when called on list with two items" $
        toTriples ['a', 'b'] `shouldBe` [('b', 'a', 'b'), ('a', 'b', 'a')]
    it "returns expected triples when called on a `String`" $
        toTriples "abcd" `shouldBe` [('d', 'a', 'b'), ('a', 'b', 'c'),
                                     ('b', 'c', 'd'), ('c', 'd', 'a')]

testConvexHullConvexPolygons :: SpecWith ()
testConvexHullConvexPolygons = describe "Test `convexHull` for simple convex polygons" $ do
    it "works for simple triangle in expected order" $
        convexHull [Point 1 1, Point 3 1, Point 2 2] `shouldBe` [Point 1 1, Point 3 1, Point 2 2]
    it "works for simple triangle in non-standard order" $
        convexHull [Point 3 1, Point 2 2, Point 1 1] `shouldBe` [Point 1 1, Point 3 1, Point 2 2]
    it "works for simple square in standard order" $
        convexHull [Point 1 1, Point 2 2, Point 1 3, Point 0 2] `shouldBe` [Point 1 1, Point 2 2, Point 1 3, Point 0 2]
    it "works for simple square in non-standard order" $
        convexHull [Point 1 3, Point 1 1, Point 0 2, Point 2 2] `shouldBe` [Point 1 1, Point 2 2, Point 1 3, Point 0 2]
    it "works for square with points in all quadrants" $
        convexHull [Point (-1) 1, Point 1 (-1), Point 1 1, Point (-1) (-1)] `shouldBe` [Point (-1) (-1), Point 1 (-1), Point 1 1, Point (-1) 1]

testConvexHullConcavePolygons :: SpecWith ()
testConvexHullConcavePolygons = describe "Test `convexHull` for concave polygons" $ do
    it "works for polygon with single point of concavity" $
        convexHull [Point 1 1, Point 3 1, Point 2 2, Point 2 3]
            `shouldBe` [Point 1 1, Point 3 1, Point 2 3]
    it "works for polygon with two non-consecutive points of concavity" $
        convexHull [Point 1 1, Point 5 1, Point 4 2, Point 5 3, Point 1 3, Point 2 2]
            `shouldBe` [Point 1 1, Point 5 1, Point 5 3, Point 1 3]
    it "works for polygon with two consecutive points of concavity" $
        convexHull [Point 1 1, Point 5 1, Point 4 2, Point 4 3, Point 5 4, Point 1 4]
            `shouldBe` [Point 1 1, Point 5 1, Point 5 4, Point 1 4]
    it "works for polygon where second point only appears inside on 2nd pass" $
        convexHull [Point 1 1, Point 5 1, Point 3 2, Point 3 3, Point 2 5]
            `shouldBe` [Point 1 1, Point 5 1, Point 2 5]

testConvexHullEdgeCases :: SpecWith ()
testConvexHullEdgeCases = describe "Test edge cases for `convexHull`" $ do
    it "returns empty list when called on an empty list" $
        convexHull ([] :: [Point]) `shouldBe` []
    it "returns single point when called on degenerate polygon with single point" $
        convexHull [Point 1 1] `shouldBe` [Point 1 1]
    it "returns two points when called on degenerate polygon with two points" $
        convexHull [Point 1 1, Point 2 2] `shouldBe` [Point 1 1, Point 2 2]
    it "returns three points when called on degenerate polygon collapsing to a line" $
        convexHull [Point 1 1, Point 2 2, Point 3 3] `shouldBe` [Point 1 1, Point 2 2, Point 3 3]

testConvexHullProperties :: SpecWith ()
testConvexHullProperties = describe "Test `convexHull` properties using QuickCheck" $ do
    it "results in a shorter list of `Point`s" $ property $
        \ps -> length (convexHull ps) <= length ps
    it "results in a list of points all of which are in the original polygon" $ property $
        \ps -> all (`elem` ps) (convexHull ps)


testConvexHull :: Spec
testConvexHull = do
    testConvexHullEdgeCases
    testConvexHullConvexPolygons
    testConvexHullConcavePolygons
    testConvexHullProperties

spec :: Spec
spec = do
    testGoldMaster
    testPointComparisons
    testTurnDirection
    testVectorComparisons
    testToTriples
    testToTriplesProperties
    testConvexHull
