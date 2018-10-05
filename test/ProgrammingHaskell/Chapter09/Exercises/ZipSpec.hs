module ProgrammingHaskell.Chapter09.Exercises.ZipSpec where

import           ProgrammingHaskell.Chapter09.Exercises.Zip
import           Test.Hspec
import           Test.QuickCheck

myZipProperty :: [Int] -> [Int] -> Bool
myZipProperty xs ys = myZip xs ys == zip xs ys

testMyZip :: Spec
testMyZip = context "myZip" $ it "Works the same as built-in `zip`" $ property myZipProperty

myZipWithProperty :: Eq c => Fun (a, b) c -> [a] -> [b] -> Bool
myZipWithProperty (Fn2 f) xs ys = myZipWith f xs ys == zipWith f xs ys

testMyZipWith :: Spec
testMyZipWith =
    context "myZipWith" $ it "Works the same as built-in `zipWith`" $ property (myZipWithProperty :: Fun (Int, Int) Int -> [Int] -> [Int] -> Bool)

spec :: Spec
spec = do
    testMyZip
    testMyZipWith
