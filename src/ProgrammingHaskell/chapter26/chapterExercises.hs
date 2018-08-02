{-# LANGUAGE InstanceSigs #-}

import           Control.Applicative        ((<|>))
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Data.Functor.Identity

------------------------------------
-- Chapter 26 - Chapter Exercises --
------------------------------------
main :: IO ()
main = undefined

--------------------
-- Write the code --
--------------------
-- Exercise 1 - rDec
rDec
    :: Num a
    => Reader a a
rDec = ReaderT (\a -> Identity (a - 1))

-- Exercise 2 - rDec - pointfree version
rDec'
    :: Num a
    => Reader a a
rDec' = ReaderT (Identity . subtract 1)

-- Exercise 3 / 4 - rShow
rShow
    :: Show a
    => ReaderT a Identity String
rShow = ReaderT (Identity . show)

-- Exercise 5 - rPrintAndInc
rPrintAndInc
    :: (Num a, Show a)
    => ReaderT a IO a
rPrintAndInc =
    ReaderT $ \a -> do
        putStrLn $ "Hi: " ++ show a
        return (a + 1)

-- Exercise 6 - sPrintIncAccum
sPrintIncAccum
    :: (Num a, Show a)
    => StateT a IO String
sPrintIncAccum =
    StateT $ \a -> do
        putStrLn $ "Hi: " ++ show a
        return (show a, a + 1)

------------------
-- Fix the code --
-------------------
isValid :: String -> Bool
isValid v = '!' `elem` v

maybeExcite :: MaybeT IO String
maybeExcite =
    MaybeT $ do
        v <- getLine
        return $ guard (isValid v) >> pure v

doExcite :: IO ()
doExcite = do
    putStrLn "say something excite!"
    excite <- runMaybeT maybeExcite
    case excite of
        Nothing -> putStrLn "MOAR EXCITE"
        Just e  -> putStrLn ("Good, was very excite: " ++ e)
