---------------------------
-- Chapter 18 - Examples --
---------------------------

-- 18.3 - do Syntax and Monads

-- Basic `do` syntax
sequencing :: IO ()
sequencing = do
    putStrLn "blah"
    putStrLn "another thing"

-- Desugared using `>>` - same output as above
sequencing' :: IO ()
sequencing' =
    putStrLn "blah" >>
    putStrLn "another thing"

-- Desugared using '*>' - same output as above
sequencing'' :: IO ()
sequencing'' =
    putStrLn "blah" *>
    putStrLn "another thing"


-- This time we use the variable-binding version of `do`
binding :: IO ()
binding = do
    name <- getLine
    putStrLn name

-- This is the desugared form, using >>=
binding' :: IO ()
binding' =
    getLine >>= putStrLn
