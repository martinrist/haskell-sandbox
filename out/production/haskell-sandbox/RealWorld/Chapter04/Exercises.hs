module RealWorld.Chapter04.Exercises where

-- 1. Safe definitions of standard partial list functions from Prelude

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead xs = Just $ head xs

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail xs = Just $ tail xs

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast xs = Just $ last xs

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit xs = Just $ init xs


-- 2. splitWith function to split an input list on every element for 
-- which the predicate returns `False`

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith p xs = let start     = takeWhile p xs
                     end       = dropWhile p xs
                     rest      = dropWhile (not . p) end
                 in start : splitWith p rest