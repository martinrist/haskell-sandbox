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


-- Example using both sequencing and binding
bindingAndSequencing :: IO ()
bindingAndSequencing = do
    putStrLn "Please enter your name:"
    name <- getLine
    putStrLn ("Hello " ++ name)

bindingAndSequencing' :: IO ()
bindingAndSequencing' =
    putStrLn "Please enter your name:" >>
    getLine >>=
    \name -> putStrLn ("Hello " ++ name)



-- More binds => more nesting
twoBinds :: IO ()
twoBinds = do
    putStrLn "Please enter your name:"
    name <- getLine
    putStrLn "Please enter your age:"
    age <- getLine
    putStrLn ("Hello " ++ name ++ "! You are " ++ age ++ " years old")

twoBinds' :: IO ()
twoBinds' =
    putStrLn "Please enter your name:" >>
    getLine >>=
    \name ->
    putStrLn "Please enter your age:" >>
    getLine >>=
    \age ->
    putStrLn ("Hello " ++ name ++ "! You are " ++ age ++ " years old")

