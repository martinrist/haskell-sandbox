module Main where

import InDepth.Chapter02.Vocab
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    case args of
         [fname, n] -> processTextFile fname (read n)
         _ -> putStrLn "Usage: vocab-builder filename count"
