{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

-- These instances shouldn't really be orphans, but it helps
-- split the files out for didactic purposes...
{-# OPTIONS_GHC -Wno-orphans #-}

module ThinkingWithTypes.Chapter05.Exercises1 where

import ThinkingWithTypes.Chapter05.Examples1


-- Exercise 5.3-i - Implement `Ord` for `HList`

instance Ord (HList '[]) where
  compare HNil HNil = EQ

instance (Ord t, Ord (HList ts)) => Ord (HList (t ': ts)) where
  compare (a :# as) (b :# bs) = compare a b <> compare as bs


-- Exercise 5.3-ii - Implement `Show` for `HList`
instance Show (HList '[]) where
  show HNil = "HNil"

instance (Show t, Show (HList ts)) => Show (HList (t ': ts)) where
  show (a :# as) = show a <> " :# " <> show as
