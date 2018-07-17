module RealWorld.Chapter04.SplitLines where

splitLines :: String -> [String]
splitLines [] = []
splitLines cs =
    let (pre, suf) = break isLineTerminator cs
    in  pre : case suf of
                   ('\r':'\n':rest) -> splitLines rest
                   ('\r':rest)      -> splitLines rest
                   ('\n':rest)      -> splitLines rest
                   _                -> []

isLineTerminator :: Char -> Bool
isLineTerminator c = c == '\r' || c == '\n'