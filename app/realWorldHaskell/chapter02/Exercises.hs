module Exercises where

lastButOne :: [a] -> Maybe a
lastButOne xs = let len = length xs in
                    if len < 2
                       then Nothing
                       else Just $ head $ drop (len - 2) xs