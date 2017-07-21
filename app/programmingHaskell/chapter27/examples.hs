import           Debug.Trace (trace)

---------------------------------
-- Chapter 27 - Non-strictness --
---------------------------------
main :: IO ()
main = undefined


-- 27.3 - Outside in, inside out
--------------------------------

possiblyKaboom =
    \f -> f fst snd (0, undefined)


-- Lambda versions of `True` and `False`
true :: a -> a -> a
true = \a -> (\b -> a)

false :: a -> a -> a
false = \a -> (\b -> b)


possiblyKaboom' b =
    case b of
         True  -> fst tup
         False -> snd tup
    where tup = (0, undefined)


-- 27.4 - What does the other way look like?
--------------------------------------------

-- Non-strict version will only explode if you enter 'hi'
hypo :: IO ()
hypo = do
    let x :: Int
        x = undefined
    s <- getLine
    case s of
         "hi" -> print x
         _    -> putStrLn "hello"

hypo' :: IO ()
hypo' = do
    let x :: Int
        x = undefined
    s <- getLine
    case (x `seq` s) of
         "hi" -> print x
         _    -> putStrLn "hello"


-- 27.8 - trace
---------------

inc = (+1)

twice = inc . inc

howManyTimes =
    inc (trace "I got eval'd" (1 + 1))
        + twice (trace "I got eval'd" (1 + 1))

howManyTimes' =
    let onePlusOne = trace "I got eval'd" (1 + 1)
    in inc onePlusOne + twice onePlusOne

