module ThinkingWithTypes.Chapter01.ExercisesSpec where

import           ThinkingWithTypes.Chapter01.Exercises
import           Test.Hspec
import           Test.QuickCheck
import           Text.Show.Functions
import           Test.QuickCheck.Checkers

-- Exercise 1.4-i - `(a^b)^c = a^(b×c)`

type ISD = (Int, String) -> Double

testExpOfExpIsomorphism :: Spec
testExpOfExpIsomorphism =
    context "Exercise 1.4-i - `(a^b)^c = a^(b×c)`" $
        it "`to` and `from` define an isomorphism" $
            (fromExpOfExp . toExpOfExp :: ISD -> ISD) =-= id



-- Exercise 1.4-ii - `a^b x a^c = a ^ (b + c)`

type EISD = Either Int String -> Double

testProdOfExpIsomorphism :: Spec
testProdOfExpIsomorphism =
    context "Exercise 1.4-ii - `a^b x a^c = a ^ (b + c)`" $
        it "`to` and `from` define an isomorphism" $
            (fromProdOfExp . toProdOfExp :: EISD -> EISD) =-= id



-- Exercise 1.4-iii - `(a x b) ^ c = a^c x b^c`

prop_expOfProdIsomorphism :: (Eq a, Eq b) => (c -> a, c -> b) -> c -> Bool
prop_expOfProdIsomorphism (ca, cb) c =
    let (ca', cb') = (fromExpOfProd . toExpOfProd) (ca, cb)
    in  (ca c, cb c) == (ca' c, cb' c)

type ISID = (Int -> String, Int -> Double)

testExpOfProdIsomorphism :: Spec
testExpOfProdIsomorphism =
    context "Exercise 1.4-iii - `(a x b) ^ c = a^c x b^c`" $
        it "`to` and `from` define an isomorphism" $
            (fromExpOfProd . toExpOfProd :: ISID -> ISID) =-= id


spec :: Spec
spec = do
    testExpOfExpIsomorphism
    testProdOfExpIsomorphism
    testExpOfProdIsomorphism
