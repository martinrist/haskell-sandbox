import           Data.Char

----------------------------
-- Chapter 22 - Exercises --
----------------------------

main :: IO ()
main = undefined

-- Short Exercise : Warming Up

cap :: String -> String
cap = map toUpper

rev :: String -> String
rev = reverse

composed :: String -> String
composed = rev . cap

fmapped :: String -> String
fmapped = rev <$> cap

tupled :: String -> (String, String)
tupled = (,) <$> cap <*> rev

monadicTupled :: String -> (String, String)
monadicTupled = do
    revd <- rev
    capd <- cap
    return (capd, revd)

monadicTupled' :: String -> (String, String)
monadicTupled' =
    rev >>=
    \revd -> cap >>=
        \capd -> return (revd, capd)