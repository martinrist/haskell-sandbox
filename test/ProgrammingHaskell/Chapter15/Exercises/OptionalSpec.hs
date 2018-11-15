module ProgrammingHaskell.Chapter15.Exercises.OptionalSpec where

import Test.Hspec
import Data.Monoid
import ProgrammingHaskell.Chapter15.Exercises.Optional

testOptional :: Spec
testOptional = context "Monoid instance for Optional" $ do
    it "Correctly `mappend`s two `Only Sum`s" $
        Only (Sum 1) `mappend` Only (Sum 1) `shouldBe` Only (Sum 2)
    it "Correctly `mappend`s two `Only Product`s" $
        Only (Product 4) `mappend` Only (Product 2) `shouldBe` Only (Product 8)
    it "Correctly `mappend`s `Only Sum` with `Nada`" $
        Only (Sum 1) `mappend` Nada `shouldBe` Only (Sum 1)
    it "Correctly `mappend`s `Only []` with `Nada`" $
        Only [1] `mappend` Nada `shouldBe` Only [1]
    it "Correctly `mappend`s `Nada` with `Only`" $
        Nada `mappend` Only (Sum 1) `shouldBe` Only (Sum 1)
    it "Correctly `mappend`s `Nada` with `Nada`" $
        Nada `mappend` Nada `shouldBe` (Nada :: Optional (Sum Int))


spec :: Spec
spec = testOptional
