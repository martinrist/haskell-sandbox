module ProgrammingHaskell.Chapter17.Exercises.IdentitySpec where

import           ProgrammingHaskell.Chapter17.Exercises.Identity
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Classes
import           Test.Hspec.Checkers


testIdentity :: Spec
testIdentity = do
    testBatch $ monoid (undefined :: Identity String)
    testBatch $ applicative (undefined :: Identity (String, String, Int))

spec :: Spec
spec = testIdentity
