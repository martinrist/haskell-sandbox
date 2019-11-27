{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

-- These instances shouldn't really be orphans, but it helps
-- split the files out for didactic purposes...
{-# OPTIONS_GHC -Wno-orphans #-}

module ThinkingWithTypes.Chapter05.Exercises2 where

import ThinkingWithTypes.Chapter05.Examples2


-- Exercise 5.3-iii - Rewrite the `Ord` and `Show` instances in terms
-- of `All`

instance (All Eq ts, All Ord ts) => Ord (HList ts) where
  compare HNil HNil = EQ
  compare (a :# as) (b :# bs) = compare a b <> compare as bs

instance All Show ts => Show (HList ts) where
  show HNil = "HNil"
  show (a :# as) = show a <> " :# " <> show as