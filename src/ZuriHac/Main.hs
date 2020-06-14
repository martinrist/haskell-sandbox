module ZuriHac.Main where

import           ZuriHac.Pal                    ( isPalindrome )
import           Data.Char
import           Control.Applicative            ( (<|>) )

-- Video 1 - GHCi Intro

x :: Integer
x = 100

y :: Integer
y = x + 2

zuriMain :: IO ()
zuriMain = putStrLn "hello"


-- Video 2 - Types & Functions - Part 1

spell :: Integer -> String
spell int = case int of
    1 -> "one"
    2 -> "two"
    3 -> "three"
    4 -> "four"
    n -> "I don't know the number " <> show n


-- Video 3 - Basic types - Part 1


-- Video 4 - Types & Functions - Part 2

idk :: (Ord a, Num a) => a -> a
idk n = if n < 10 then negate n else n + 10

{-# HLINT ignore preferJ "Use infix" #-}
preferJ :: Foldable t => t Char -> t Char -> t Char
preferJ a b = if elem 'j' a then a else b


-- Video 5 - Basic types - Part 2

{-# HLINT ignore idk2 "Use if" #-}
idk2 :: (Ord a, Num a) => a -> a
idk2 z = case z < 10 of
    True  -> negate z
    False -> z + 10


-- Session 2 / 3 - Palindromes
-- Interactive Program

main :: IO ()
main = do
    word <- getLine
    print $ verbose word

verbose :: String -> String
verbose word = case isPalindrome word of
    Nothing    -> "Please enter a word"
    Just False -> "This isn't a palindrome"
    Just True  -> "This is a palindrome"



-- Session 3

example1 :: Maybe String
example1 = Nothing

example2 :: Maybe String
example2 = Just "Alonzo"


-- Functors

database :: [(Integer, String)]
database = [ (1, "Julie"),
             (2, "Chris"),
             (3, "Alonzo"),
             (4, "Melman"),
             (5, "B@*$**S"),
             (6, "skdjfhjksdfhk h9ewuf iuwehfiwuefhiuefh iueh eiufh ew")]

greetUser :: Integer -> Maybe String
greetUser record =
    generalMap ("Hello " ++) $ lookup record database >>= makeUsername

mapToMaybe :: (a -> b) -> Maybe a -> Maybe b
mapToMaybe _ Nothing  = Nothing
mapToMaybe f (Just a) = Just $ f a

mapToEither :: (a -> b) -> (Either e) a -> (Either e) b
mapToEither _ (Left  l) = Left l
mapToEither f (Right r) = Right $ f r

class Mappable m where
    generalMap :: (a -> b) -> m a -> m b

instance Mappable Maybe where
    generalMap = fmap


-- Monads

rejectNonAlphabetic :: String -> Maybe String
rejectNonAlphabetic string =
    if all isAlpha string then Just string else Nothing

removeSpaces :: String -> Maybe String
removeSpaces string = case filter (/= ' ') string of
    ""     -> Nothing
    result -> Just result

validateLength :: String -> Maybe String
validateLength string = if length string > 25 then Nothing else Just string

{-# HLINT ignore makeUsername "Replace case with maybe" #-}
makeUsername :: String -> Maybe String
makeUsername string =
    case removeSpaces string of
         Nothing -> Nothing
         Just xs ->
             case rejectNonAlphabetic xs of
                Nothing -> Nothing
                Just xs' ->
                    case validateLength xs' of
                         Nothing -> Nothing
                         Just xs'' -> Just xs''

makeUsername' :: String -> Maybe String
makeUsername' string = case removeSpaces string of
    Nothing -> Nothing
    Just xs -> case rejectNonAlphabetic xs of
        Nothing  -> Nothing
        Just xs' -> validateLength xs' <|> Nothing

makeUsername'' :: String -> Maybe String
makeUsername'' string = do
    string' <- removeSpaces string
    string'' <- rejectNonAlphabetic string'
    validateLength string''

makeUsername''' :: String -> Maybe String
makeUsername''' string = removeSpaces string >>= rejectNonAlphabetic >>= validateLength
