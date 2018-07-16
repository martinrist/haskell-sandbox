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


spec :: Spec
spec = do
    testGoldMaster
