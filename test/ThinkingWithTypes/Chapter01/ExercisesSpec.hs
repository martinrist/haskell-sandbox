module ThinkingWithTypes.Chapter01.ExercisesSpec where

import           ThinkingWithTypes.Chapter01.Exercises
import           Test.Hspec
import           Test.QuickCheck
import           Text.Show.Functions
import           Test.QuickCheck.Checkers

-- Exercise 1.4-i - `(a^b)^c = a^(b×c)`

-- a ~ Double
-- b ~ Int
-- c ~ String
type TISD = (Int, String) -> Double
type ISD  = Int -> String -> Double

testExpOfExpIsomorphism :: Spec
testExpOfExpIsomorphism =
    context "Exercise 1.4-i - `(a^b)^c = a^(b×c)`" $ do
        it "`from . to == id`" $
            (fromExpOfExp . toExpOfExp :: TISD -> TISD) =-= id
        it "`to . from == id`" $
            (toExpOfExp . fromExpOfExp :: ISD -> ISD) =-= id



-- Exercise 1.4-ii - `a^b x a^c = a ^ (b + c)`

-- a ~ Double
-- b ~ Int
-- c ~ String
type EISD  = Either Int String -> Double
type TIDSD = (Int -> Double, String -> Double)

testProdOfExpIsomorphism :: Spec
testProdOfExpIsomorphism =
    context "Exercise 1.4-ii - `a^b x a^c = a ^ (b + c)`" $ do
        it "`from . to == id`" $
            (fromProdOfExp . toProdOfExp :: EISD -> EISD) =-= id
        it "`to . from == id`" $
            (toProdOfExp . fromProdOfExp :: TIDSD -> TIDSD) =-= id



-- Exercise 1.4-iii - `(a x b) ^ c = a^c x b^c`

-- a ~ Double
-- b ~ Int
-- c ~ String
type TSDSI = (String -> Double, String -> Int)
type STDI  = String -> (Double, Int)

testExpOfProdIsomorphism :: Spec
testExpOfProdIsomorphism =
    context "Exercise 1.4-iii - `(a x b) ^ c = a^c x b^c`" $ do
        it "`from . to == id`" $
            (fromExpOfProd . toExpOfProd :: TSDSI -> TSDSI) =-= id
        it "`to . from == id`" $
            (toExpOfProd . fromExpOfProd :: STDI -> STDI) =-= id



spec :: Spec
spec = do
    testExpOfExpIsomorphism
    testProdOfExpIsomorphism
    testExpOfProdIsomorphism
