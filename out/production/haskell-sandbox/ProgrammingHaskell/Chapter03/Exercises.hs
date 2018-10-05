module ProgrammingHaskell.Chapter03.Exercises where

-- Exercise 2
exercise_2a :: String -> String
exercise_2a s = s ++ "!"

exercise_2b :: String -> Char
exercise_2b s = s !! 4

exercise_2c :: String -> String
exercise_2c s = drop 9 s
-- Note that we can remove the trailling `s`
exercise_2c' :: String -> String
exercise_2c' = drop 9


-- Exercise 3
thirdLetter :: String -> Char
thirdLetter s = s !! 2


-- Exercise 4
letterIndex :: Int -> Char
letterIndex x = "Curry is awesome" !! x


-- Exercise 5
rvrs :: String -> String
rvrs s = let word1 = take 5 s;
             word2 = take 2 (drop 6 s);
             word3 = take 7 (drop 9 s)
         in concat [word3, " ", word2, " ", word1]