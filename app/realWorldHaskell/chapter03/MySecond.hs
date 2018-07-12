module MySecond where

-- Illustrates the use of Maybe to handle failures

safeSecond :: [a] -> Maybe a
safeSecond (_:x:_) = Just x
safeSecond _       = Nothing