{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}

module ThinkingWithTypes.Chapter06.Exercises where


newtype Cont a = Cont
  { unCont :: forall r. (a -> r) -> r
  }


-- Exercise 6.4-i - Implement `Functor` for `Cont`

instance Functor Cont where
  fmap :: (a -> b) -> Cont a -> Cont b
  -- Step 1 - we need to return a `Cont b`
  -- fmap ab (Cont arr) = Cont $ _                 -- _ :: (b -> r) -> r

  -- Step 2 - start creating a lambda
  -- fmap ab (Cont arr) = Cont $ \br -> _          -- _ :: r

  -- Step 3 - How can we produce an `r`?  Use `arr :: (a -> r) -> r`
  -- fmap ab (Cont arr) = Cont $ \br -> arr _      -- _ :: a -> r

  -- Step 4 - We need to produce an `a -> r` and haven't
  -- used either `ab :: a -> b` or `br :: b -> r` yet,
  -- `br . ab :: a -> r` gives us what we need
  fmap ab (Cont arr) = Cont $ \br -> arr (br . ab)


-- Exercise 6.4-ii - Implement `Applicative` for `Cont`

instance Applicative Cont where
  pure a            = Cont $ \c -> c a
  Cont f <*> Cont v = Cont $ \br -> f $ \ab -> v (br . ab)


-- Exercise 6.4-iii - Implement 'Monad' for 'Cont'

instance Monad Cont where
  return = pure
  Cont m >>= f = Cont $ \br -> m $ \ a -> unCont (f a) br