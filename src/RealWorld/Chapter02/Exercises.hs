module RealWorld.Chapter02.Exercises where

{-# HLINT ignore lastButOne "Use !!" #-}
lastButOne :: [a] -> Maybe a
lastButOne xs = let len = length xs in
                    if len < 2
                       then Nothing
                       else Just $ head $ drop (len - 2) xs