module ProgrammingHaskell.Chapter08.Exercises.Mc91 where

import Test.Hspec

mc91 :: Int -> Int
mc91 n
    | n > 100 = n - 10
    | otherwise = mc91 (mc91 (n + 11))

main :: IO ()
main =
    hspec $
    describe "McCarthy 91 function" $
    it "Returns expected results" $
    map mc91 [95 .. 110] `shouldBe`
    [91, 91, 91, 91, 91, 91, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100]