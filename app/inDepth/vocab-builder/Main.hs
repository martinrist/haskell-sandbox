module Main where

import InDepth.Chapter02.Vocab
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    case args of
         [fname] -> processTextFile fname
         _ -> putStrLn "Usage: vocab-builder filename"
