{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

module ThinkingWithTypes.Chapter02.Examples where


type family Or (x :: Bool) (y :: Bool) :: Bool where
    Or 'True  y = 'True
    Or 'False y = y

type family Map (x :: a -> b) (i :: [a]) :: [b] where
    Map f '[]       = '[]
    Map f (x ': xs) = f x ': Map f xs