module ProgrammingHaskell.Chapter16.Exercises.SumSpec where

import           ProgrammingHaskell.Chapter16.Exercises.Sum
import           ProgrammingHaskell.Chapter16.FunctorLaws
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Function


testSum :: Spec
testSum = context "Sum" $ do
    it "Obeys Functor Possibly law" $
        property (functorIdentity :: Sum String Int -> Bool)
    it "Obeys Functor composition law" $
        property (functorCompose :: Sum String Int -> Fun Int String -> Fun String Int -> Bool)


spec :: Spec
spec =
    testSum
