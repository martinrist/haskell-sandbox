{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module ThinkingWithTypes.Chapter2.Exercises where

type family Not (x :: Bool) :: Bool where
    Not 'True  = 'False
    Not 'False = 'True