{-# LANGUAGE InstanceSigs #-}

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe

-------------------------------------
-- Chapter 26 - Monad Transformers --
-------------------------------------
main :: IO ()
main = undefined

embedded :: MaybeT (ExceptT String []) Int
embedded = return 1

maybeUnwrap :: ExceptT String [] (Maybe Int)
maybeUnwrap = runMaybeT embedded

eitherUnwrap :: [Either String (Maybe Int)]
eitherUnwrap = runExceptT maybeUnwrap
