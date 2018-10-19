module Main where

import ProgrammingHaskell.Chapter13.Hangman
import Data.Char

main :: IO ()
main = do
    word <- randomWord'
    let puzzle = freshPuzzle (fmap toLower word)
    runGame puzzle
