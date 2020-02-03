{-# LANGUAGE RankNTypes #-}

module ThinkingWithTypes.Chapter06.Examples where

-- applyToFiveBroken :: (a -> a) -> Int
-- This won't compile, because we can't guarantee that `f` can take an `Int`
-- It's equivalent to either of:
-- applyToFiveBroken :: forall a. (a -> a) -> Int
-- applyToFiveBroken :: forall a. ( (a -> a) -> Int )
-- applyToFiveBroken f = f 5

-- Moving the quantifier so that it just applies to the first parameter
-- makes it work:
applyToFive :: (forall a. a -> a) -> Int
applyToFive f = f 5