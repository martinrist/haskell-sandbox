{-# LANGUAGE ExistentialQuantification #-}

import Control.Exception
import Data.Typeable

---------------------------------------
-- Chapter 30 - When things go wrong --
---------------------------------------
main :: IO ()
main = undefined

data MyException =
    forall e .
    (Show e, Typeable e) => MyException e

instance Show MyException where
    showsPrec p (MyException e) =
        showsPrec p e

multiError :: Int -> Either MyException Int
multiError n =
    case n of
         0 -> Left (MyException DivideByZero)
         1 -> Left (MyException StackOverflow)
         _ -> Right n

data SomeError =
    Arith ArithException
  | Async AsyncException
  | SomethingElse
  deriving (Show)

discriminateError :: MyException -> SomeError
discriminateError (MyException e) =
    case cast e of
         (Just arith) -> Arith arith
         Nothing ->
             case cast e of
                  (Just async) -> Async async
                  Nothing -> SomethingElse

runDisc n =
    either discriminateError
    (const SomethingElse) (multiError n)


-- 30.3 - Handling Exceptions
-----------------------------

fileExample :: IO ()
fileExample = do
    writeFile "aaa" "hi"
    putStrLn "wrote to file"

handler :: SomeException -> IO ()
handler (SomeException e) = do
    print (typeOf e)
    putStrLn ("We errored! It was: " ++ show e)

fileExampleWithCatch :: IO ()
fileExampleWithCatch =
    writeFile "zzz" "hi"
        `catch` handler


-- 30.4 - Want either? Try!
---------------------------

willIFail :: Integer -> IO (Either ArithException ())
willIFail denom =
    try $ print $ div 5 denom



-- 30.5 - The unbearable imprecision of trying
----------------------------------------------

canICatch :: Exception e
          => e
          -> IO (Either ArithException ())
canICatch e =
    try $ throwIO e



-- 30.7 - Making our own exception type
---------------------------------------

data NotDivThree =
    NotDivThree Int
    deriving (Eq, Show)

instance Exception NotDivThree

data NotEven =
    NotEven Int
    deriving (Eq, Show)

instance Exception NotEven

evenAndThreeDiv :: Int -> IO Int
evenAndThreeDiv i
    | rem i 3 /= 0 = throwIO (NotDivThree i)
    | odd i        = throwIO (NotEven i)
    | otherwise    = return i

catchBoth :: IO Int -> IO Int
catchBoth ioInt =
    catches ioInt
    [ Handler (\(NotEven _)     -> return maxBound)
    , Handler (\(NotDivThree _) -> return minBound)
    ]


