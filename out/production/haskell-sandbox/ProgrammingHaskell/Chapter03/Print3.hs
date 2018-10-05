module ProgrammingHaskell.Chapter03.Print3 where

myGreeting :: String
-- ++ can be used to concatenate Strings
myGreeting = "hello" ++ " world!"

hello :: String
hello = "hello"

world :: String
world = "world!"

main :: IO ()
main = do
    putStrLn myGreeting
    putStrLn secondGreeting
    -- concat can be used for more general concatenation of a `Foldable`
    -- (here a list of [Char])
    where secondGreeting = concat [hello, " ", world]
