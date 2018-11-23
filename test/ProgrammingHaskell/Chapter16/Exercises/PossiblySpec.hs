module ProgrammingHaskell.Chapter16.Exercises.PossiblySpec where

import           ProgrammingHaskell.Chapter16.Exercises.Possibly
import           ProgrammingHaskell.Chapter16.FunctorLaws
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Function


testPossibly :: Spec
testPossibly = context "Possibly" $ do
    it "Obeys Functor Possibly law" $
        property (functorIdentity :: Possibly Int -> Bool)
    it "Obeys Functor composition law" $
        property (functorCompose :: Possibly Int -> Fun Int String -> Fun String Int -> Bool)


spec :: Spec
spec =
    testPossibly
