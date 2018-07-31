{-# LANGUAGE OverloadedStrings #-}

module InDepth.Chapter02.Vocab where

import Data.Char
import Data.List
import Data.Ord
import qualified Data.Text as T
import qualified Data.Text.IO as TIO



-- | A single entry, consisting of a word and its number of occurrences
type Entry = (T.Text, Int)

-- | A list of entries
type Vocabulary = [Entry]


-- | Extracts a `Vocabulary` from a piece of text
extractVocab :: T.Text -> Vocabulary
extractVocab t = map buildEntry $ group $ sort wrds
    where
        wrds = map T.toCaseFold $ filter (not . T.null) $ map cleanWord $ T.words t
        buildEntry ws@(w:_) = (w, length ws)
        cleanWord = T.dropAround (not . isLetter)


-- | Produces a report containing all the words
allWordsReport :: Vocabulary -> T.Text
allWordsReport vocab = T.append "\nAll words:\n"
                     $ T.unlines $ map fst vocab

-- | Produces a report of the total word count
wordsCountReport :: Vocabulary -> T.Text
wordsCountReport vocab = T.append "\nTotal number of words: "
                         $ T.pack $ show $ wordsCount vocab

-- | Produces a report of the most frequently-used words
frequentWordsReport :: Vocabulary -> Int -> T.Text
frequentWordsReport vocab n = T.append "\nFrequent Words:\n"
                            $ T.unlines $ map showEntry $ take n
                            $ wordsByFrequency vocab
    where
        showEntry (t, c) = T.append t $ T.pack $ " - " ++ show c

-- | Counts the total number of words in the `Vocabulary`
wordsCount :: Vocabulary -> Int
wordsCount vocab = sum $ map snd vocab

-- | Sorts the `Vocabulary` in decreasing order of word frequency
wordsByFrequency :: Vocabulary -> Vocabulary
wordsByFrequency = sortBy (comparing $ Down . snd)


-- | Prints out all the words in a `Vocabulary`
printAllWords :: Vocabulary -> IO ()
printAllWords vocab = do
    putStrLn "All words: "
    TIO.putStrLn $ T.unlines $ map fst vocab

-- | Processes a text file at a given path and prints out words
processTextFile :: FilePath -> Int -> IO ()
processTextFile fname n = do
    text <- TIO.readFile fname
    let vocab = extractVocab text
    TIO.putStrLn $ allWordsReport vocab
    TIO.putStrLn $ wordsCountReport vocab
    TIO.putStrLn $ frequentWordsReport vocab n

