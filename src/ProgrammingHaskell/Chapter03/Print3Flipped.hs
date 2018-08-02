module ProgrammingHaskell.Chapter03.Print3Flipped where

myGreeting :: String
-- Here `(++)` denotes the prefix version of the `++` operator
myGreeting = (++) "hello" " world!"

hello :: String
hello = "hello"

world :: String
world = "world!"

main :: IO ()
main = do
    putStrLn myGreeting
    putStrLn secondGreeting
    -- `secondGreeting` is a local definition, not visible outside `main`
    where secondGreeting = (++) hello ((++) " " world)
