module ProgrammingHaskell.Chapter13.Main where

import System.IO

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStr "Please enter your name: "
    name <- getLine
    sayHello name

sayHello :: String -> IO ()
sayHello name = putStrLn ("Hi " ++ name ++ "!")


getAndConcat :: IO String
getAndConcat = do
    x1 <- getLine
    x2 <- getLine
    return (x1 ++ x2)