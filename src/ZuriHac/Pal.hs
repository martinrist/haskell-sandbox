module ZuriHac.Pal where

import           Data.Char

-- What a palindrome is
isPalindrome :: String -> Maybe Bool
isPalindrome = isMaybeOwnReverse . rejectEmpty . normalise

rejectEmpty :: String -> Maybe String
rejectEmpty [] = Nothing
rejectEmpty s  = Just s

normalise :: String -> String
normalise = removePunctuation . removeSpaces . allLowerCase

isMaybeOwnReverse :: Maybe String -> Maybe Bool
isMaybeOwnReverse Nothing  = Nothing
isMaybeOwnReverse (Just s) = Just $ s == reverse s

removePunctuation :: String -> String
removePunctuation = filter $ not . isPunctuation

removeSpaces :: String -> String
removeSpaces = filter (/= ' ')

allLowerCase :: String -> String
allLowerCase = map toLower



-- folds
{-# HLINT ignore "Use if" #-}
rejectNonAlphabetic :: String -> Maybe String
rejectNonAlphabetic string = case myAll isAlpha string of
    False -> Nothing
    True  -> Just string

myAll :: (a -> Bool) -> [a] -> Bool
myAll f = Prelude.foldr (\x y -> f x && y) True


foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ acc []       = acc
foldr f acc (x : xs) = f x (ZuriHac.Pal.foldr f acc xs)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ acc []       = acc
foldl f acc (x : xs) = ZuriHac.Pal.foldl f (f acc x) xs
