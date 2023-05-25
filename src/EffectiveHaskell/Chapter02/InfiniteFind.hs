{-# OPTIONS_GHC -Wno-missing-signatures #-}
module EffectiveHaskell.Chapter02.InfiniteFind where

import Prelude hiding (foldl, foldr)

-- This is our custom version of `Prelude.foldl`
foldl func carryValue lst =
    if null lst
        then carryValue
        else foldl func (func carryValue (head lst)) (tail lst)

-- This is our custom version of `Prelude.foldr`
foldr func carryValue lst =
    if null lst
        then carryValue
        else func (head lst) $ foldr func carryValue (tail lst)

findFirst predicate =
    foldr findHelper []
    where
        findHelper listElement maybeFound
            | predicate listElement = [listElement]
            | otherwise = maybeFound
