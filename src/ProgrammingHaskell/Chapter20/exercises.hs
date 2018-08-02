import Data.Monoid

----------------------------
-- Chapter 20 - Exercises --
----------------------------

main :: IO ()
main = undefined


-- Exercises : Library Functions
--------------------------------

-- All the functions here are suffixed with ', to prevent collisions with
-- the corresponding functions in `Prelude`.

-- 1 - sum
sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum


-- 2 - product
product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product


-- 3 - elem
elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' e = getAny . foldMap (Any . (== e))


-- 4 - minimum
minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = foldr maybeSmaller Nothing
    where maybeSmaller x Nothing = Just x
          maybeSmaller x (Just y)
              | x < y = Just x
              | otherwise = Just y


-- 5 - maximum
maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = foldr maybeLarger Nothing
    where maybeLarger x Nothing = Just x
          maybeLarger x (Just y)
              | x > y = Just x
              | otherwise = Just y


-- 6 - null
null' :: Foldable t => t a -> Bool
null' = foldr alwaysFalse True
    where alwaysFalse _ _ = False


-- 7 - length
length' :: Foldable t => t a -> Int
length' = getSum . foldMap (Sum . const 1)


-- 8 - toList
toList' :: Foldable t => t a -> [a]
toList' = foldr (:) []


-- 9 - fold
fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id


-- 10 - foldMap in terms of foldr
foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (mappend . f) mempty
