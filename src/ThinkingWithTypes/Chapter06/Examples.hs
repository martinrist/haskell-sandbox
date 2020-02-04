{-# LANGUAGE RankNTypes #-}

module ThinkingWithTypes.Chapter06.Examples where

import ThinkingWithTypes.Chapter06.Exercises

-- applyToFiveBroken :: (a -> a) -> Int
-- This won't compile, because we can't guarantee that `f` can take an `Int`
-- It's equivalent to either of:
-- applyToFiveBroken :: forall a. (a -> a) -> Int
-- applyToFiveBroken :: forall a. ( (a -> a) -> Int )
-- applyToFiveBroken f = f 5

-- Moving the quantifier so that it just applies to the first parameter
-- makes it work:
applyToFive :: (forall a. a -> a) -> Int
applyToFive f = f 5


cont :: a -> (forall r. (a -> r) -> r)
cont a callback = callback a

runCont :: (forall r. (a -> r) -> r) -> a
runCont f =
  let callback = id
    in f callback



-- The `Cont` monad in action.  These functions all 'perform async operations'
-- then call their given callbacks when completed

withVersionNumber :: (Double -> r) -> r
withVersionNumber f = f 1.0

withTimestamp :: (Int -> r) -> r
withTimestamp f = f 1532083362

withOS :: (String -> r) -> r
withOS f = f "linux"


-- This is the 'pyramid-of-doom' style of combining these three functions:

releaseString :: String
releaseString =
  withVersionNumber $ \version ->
    withTimestamp $ \date ->
      withOS $ \os ->
        os ++ "-" ++ show version ++ "-" ++ show date

releaseStringCont :: String
releaseStringCont = runCont $ unCont $ do
  version <- Cont withVersionNumber
  date <- Cont withTimestamp
  os <- Cont withOS
  pure $ os ++ "-" ++ show version ++ "-" ++ show date