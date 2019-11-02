{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module ThinkingWithTypes.Chapter02.Exercises where

type family Not (x :: Bool) :: Bool where
    Not 'True  = 'False
    Not 'False = 'True