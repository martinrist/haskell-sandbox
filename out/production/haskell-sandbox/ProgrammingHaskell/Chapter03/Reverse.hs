module ProgrammingHaskell.Chapter03.Reverse where

rvrs :: String -> String
rvrs s = let word1 = take 5 s;
             word2 = take 2 (drop 6 s);
             word3 = take 7 (drop 9 s)
             in concat [word3, " ", word2, " ", word1]

main :: IO ()
-- Note that here we can use `$` to avoid extra parens.  This means
-- that `rvrs "..."` is evaluated first.
main = print $ rvrs "Curry is awesome!"