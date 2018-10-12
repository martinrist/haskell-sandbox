module ProgrammingHaskell.Chapter12.Exercises.Maybe where

-- Question 1
isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True

isNothing :: Maybe a -> Bool
isNothing = not . isJust

-- Question 2
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee y _ Nothing = y
mayybee _ f (Just x) = f x

-- Question 3
fromMaybe :: a -> Maybe a -> a
fromMaybe a Nothing = a
fromMaybe _ (Just x) = x

-- Question 4
listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe xs = Just $ head xs

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

-- Question 5
catMaybes :: [Maybe a] -> [a]
-- catMaybes [] = []
-- catMaybes (Nothing:xs) = catMaybes xs
-- catMaybes (Just x:xs) = x : catMaybes xs
catMaybes = fromMaybe [] . sequence . filter isJust

-- Question 6
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Just []
flipMaybe (Nothing:_) = Nothing
flipMaybe (Just x:xs) =
    case flipMaybe xs of
        Nothing -> Nothing
        Just ys -> Just (x : ys)