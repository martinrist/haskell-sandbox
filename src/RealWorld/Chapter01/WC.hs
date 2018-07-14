module RealWorld.Chapter01.WC where

main = interact wordCount
  where
    wordCount input = show (length (lines input)) ++ "\n"