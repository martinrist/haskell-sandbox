module ThinkingWithTypes.Chapter05.Exercises2Spec where

import           ThinkingWithTypes.Chapter05.Exercises2()
import           ThinkingWithTypes.Chapter05.Examples2
import           Test.Hspec

testOrdForHList :: Spec
testOrdForHList =
    context "Exercise 5.3-iii - `Ord` for `HList`" $ do
        it "Compares `HNil` correctly" $
            compare HNil HNil `shouldBe` EQ
        it "Compares single element list correctly" $ do
            compare (42 :# HNil) (42 :# HNil) `shouldBe` EQ
            compare (42 :# HNil) (43 :# HNil) `shouldBe` LT
            compare (43 :# HNil) (42 :# HNil) `shouldBe` GT
        it "Compares two element lists correctly" $ do
            compare (42 :# False :# HNil) (43 :# False :# HNil) `shouldBe` LT
            compare (42 :# False :# HNil) (42 :# True :# HNil) `shouldBe` LT
            compare (42 :# False :# HNil) (42 :# False :# HNil) `shouldBe` EQ

testShowForHList :: Spec
testShowForHList =
    context "Exercise 5.3-iii - `Show` for `HList`" $ do
        it "Produces correct description for HNil" $
            show HNil `shouldBe` "HNil"
        it "Produces correct description for single element list" $ do
            show (42 :# HNil) `shouldBe` "42 :# HNil"
            show (True :# HNil) `shouldBe` "True :# HNil"
        it "Produces correct description for two-element list" $
            show (42 :# False :# HNil) `shouldBe` "42 :# False :# HNil"

spec :: Spec
spec = do
    testOrdForHList
    testShowForHList
