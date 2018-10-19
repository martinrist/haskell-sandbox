module Main where

import           ProgrammingHaskell.Chapter13.Hangman
import           Control.Monad                  ( forever )
import           Data.Maybe                     ( isJust )
import           System.Exit                    ( exitSuccess )
import           Data.Char

main :: IO ()
main = do
    word <- randomWord'
    let puzzle = freshPuzzle (fmap toLower word)
    runGame puzzle

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed) = if length guessed > 7
    then do
        putStrLn "You lose!"
        putStrLn $ "The word was: " ++ wordToGuess
        exitSuccess
    else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) = if all isJust filledInSoFar
    then do
        putStrLn "You win!"
        exitSuccess
    else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
    gameOver puzzle
    gameWin puzzle
    putStrLn $ "Current puzzle is: " ++ show puzzle
    putStr "Guess a letter: "
    guess <- getLine
    case guess of
        [c] -> handleGuess puzzle c >>= runGame
        _   -> putStrLn "Your guess must be a single character"
