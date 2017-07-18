{-# LANGUAGE InstanceSigs #-}

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
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

rDec :: Num a => Reader a a
rDec = ReaderT (\a -> Identity (a - 1))


-- Exercise 2 - rDec - pointfree version
rDec' :: Num a => Reader a a
rDec' = ReaderT (Identity . subtract 1)


-- Exercise 3 / 4 - rShow

rShow :: Show a => ReaderT a Identity String
rShow = ReaderT (Identity . show)


-- Exercise 5 - rPrintAndInc

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = ReaderT $ \a -> do
    putStrLn $ "Hi: " ++ show a
    return (a + 1)


-- Exercise 6 - sPrintIncAccum
sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = StateT $ \a -> do
    putStrLn $ "Hi: " ++ show a
    return (show a, a + 1)
