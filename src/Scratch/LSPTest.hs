{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# LANGUAGE TypeApplications #-}

-- This module contains various expressions and functions that can be used
-- to test out haskell-language-server

module Scratch.LSPTest where

-- 'Evaluate' code lens

-- >>> 3 + 5

doubleMe :: Int -> Int
doubleMe x = x + x

-- >>> doubleMe 3

-- >>> :type foo @Int

-- >>> :type +v foo @Int

-- >>> :kind (->)


-- HLint integration

{-# HLINT ignore foo "Eta reduce" #-}
{-# HLINT ignore foo "Redundant id" #-}
foo :: a -> a
foo x = id x

bar :: a -> a
bar x = id x


-- Hole filling

fmapEither :: (a -> b) -> Either c a -> Either c b
-- Replace `undefined` with `_` to set up a hole
fmapEither fab eca = undefined