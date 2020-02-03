module ThinkingWithTypes.Chapter06.ExercisesSpec where

import           ThinkingWithTypes.Chapter06.Exercises()
import           Test.Hspec

testNotImplemented :: Spec
testNotImplemented =
    context "Not implemented" $
        it "Isn't implemented yet" $
            pendingWith "Not implemented yet"

spec :: Spec
spec =
    testNotImplemented
