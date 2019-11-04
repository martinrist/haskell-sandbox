module ThinkingWithTypes.Chapter01.ExercisesSpec where

import           ThinkingWithTypes.Chapter01.Exercises
import           Test.Hspec
import           Test.QuickCheck
import           Text.Show.Functions

-- Exercise 1.4-i - `(a^b)^c = a^(b×c)`

prop_expOfExpIsomorphism :: Eq a => ((b, c) -> a) -> b -> c -> Bool
prop_expOfExpIsomorphism fbca b c =
    fbca (b, c) == (fromExpOfExp . toExpOfExp) fbca (b, c)

testExpOfExpIsomorphism :: Spec
testExpOfExpIsomorphism =
    context "Exercise 1.4-i - `(a^b)^c = a^(b×c)`"
        $ it "`to` and `from` define an isomorphism"
        $ property
              (prop_expOfExpIsomorphism :: ((Int, String) -> Double)
                -> Int
                -> String
                -> Bool
              )


-- Exercise 1.4-ii - `a^b x a^c = a ^ (b + c)`

prop_prodOfExpIsomorphism :: Eq a => (Either b c -> a) -> Either b c -> Bool
prop_prodOfExpIsomorphism fbca ebc =
    fbca ebc == (fromProdOfExp . toProdOfExp) fbca ebc

testProdOfExpIsomorphism :: Spec
testProdOfExpIsomorphism =
    context "Exercise 1.4-ii - `a^b x a^c = a ^ (b + c)`"
        $ it "`to` and `from` define an isomorphism"
        $ property
              (prop_prodOfExpIsomorphism :: (Either Int String -> Double)
                -> Either Int String
                -> Bool
              )



-- Exercise 1.4-iii - `(a x b) ^ c = a^c x b^c`

prop_expOfProdIsomorphism :: (Eq a, Eq b) => (c -> a, c -> b) -> c -> Bool
prop_expOfProdIsomorphism (ca, cb) c =
    let (ca', cb') = (fromExpOfProd . toExpOfProd) (ca, cb)
    in  (ca c, cb c) == (ca' c, cb' c)


testExpOfProdIsomorphism :: Spec
testExpOfProdIsomorphism =
    context "Exercise 1.4-iii - `(a x b) ^ c = a^c x b^c`"
        $ it "`to` and `from` define an isomorphism"
        $ property
              (prop_expOfProdIsomorphism :: (Int -> String, Int -> Double)
                -> Int
                -> Bool
              )


spec :: Spec
spec = do
    testExpOfExpIsomorphism
    testProdOfExpIsomorphism
    testExpOfProdIsomorphism
