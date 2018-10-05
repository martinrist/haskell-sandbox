module ProgrammingHaskell.Chapter07.Exercises.ChapterExercises where

import Test.Hspec

-- Question 1
-- Note that this doesn't work for negative numbers
tensDigit' :: Integral a => a -> a
tensDigit' x = d
  where
    xLast = x `div` 10
    d = xLast `mod` 10

tensDigit :: Integral a => a -> a
tensDigit x = d
  where
    (d1, _) = x `divMod` 10
    (_, d) = d1 `divMod` 10

question1Tests :: Spec
question1Tests =
    describe "Question 1 - `tensDigit`" $ do
        it "x < 10" $ tensDigit 9 `shouldBe` 0
        it "x = 10" $ tensDigit 10 `shouldBe` 1
        it "10 < x < 100" $ tensDigit 42 `shouldBe` 4
        it "x = 100" $ tensDigit 100 `shouldBe` 0
        it "x > 100" $ tensDigit 12345 `shouldBe` 4
        it "x = 0" $ tensDigit 0 `shouldBe` 0

-- Question 2
foldBool1 :: a -> a -> Bool -> a
foldBool1 x y b =
    case b of
        False -> x
        True -> y

foldBool2 :: a -> a -> Bool -> a
foldBool2 x y b
    | b = y
    | otherwise = x

foldBool3 :: a -> a -> Bool -> a
foldBool3 x _ False = x
foldBool3 _ y True = y

question2Tests :: Spec
question2Tests =
    let foldBool = foldBool2
    in describe "Question 2 - `foldBool`" $ do
           it "False returns first argument" $
               foldBool "foo" "bar" False `shouldBe` "foo"
           it "True returns second argument" $
               foldBool "foo" "bar" True `shouldBe` "bar"

-- Question 3
g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)

question3Tests :: Spec
question3Tests =
    describe "Question 3 - `g`" $
    it "Applies function to first tuple component" $
    g (+ 1) (1, "foo") == (2, "foo")

-- Question 4
roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

-- Question 5
roundTripPF :: (Show a, Read a) => a -> a
roundTripPF = read . show

-- Question 6
roundTrip' :: (Show a, Read b) => a -> b
roundTrip' = read . show

main :: IO ()
main =
    hspec $ do
        question1Tests
        question2Tests
        question3Tests