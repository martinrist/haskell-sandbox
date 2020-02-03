{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}

module ThinkingWithTypes.Chapter06.Exercises where


newtype Cont a = Cont
  { unCont :: forall r. (a -> r) -> r
  }


-- Exercise 6.4-i - Implement `Functor` for `Cont`

instance Functor Cont where
  fmap :: (a -> b) -> Cont a -> Cont b
  fmap f (Cont arr) = Cont $ \br -> arr (br . f)


-- Exercise 6.4-ii - Implement `Applicative` for `Cont`

instance Applicative Cont where
  pure a            = Cont $ \c -> c a
  Cont f <*> Cont v = Cont $ \br -> f $ \ab -> v (br . ab)


-- Exercise 6.4-iii - Implement 'Monad' for 'Cont'

instance Monad Cont where
  return = pure
  Cont m >>= f = Cont $ \br -> m $ \ a -> unCont (f a) br