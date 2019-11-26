{-# LANGUAGE GADTs #-}

-- This allows us to automatically derive `Show` for `Expr a`
{-# LANGUAGE StandaloneDeriving #-}



-- Extensions for the definition of `HList`:

-- This allows us to use the kind `[Type]` and the type `'[]`
{-# LANGUAGE DataKinds #-}

-- This allows us to give `ts` a kind signature in the definition of `HList`
{-# LANGUAGE KindSignatures #-}

-- This allows us to use `':` in the definition of `HList`
{-# LANGUAGE TypeOperators #-}

-- This is needed for the instance declarations
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}


-- {-# LANGUAGE ConstraintKinds #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE TypeApplications #-}
-- {-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE UndecidableInstances #-}


module ThinkingWithTypes.Chapter05.Examples where

-- This allows us to refer to the kind of `ts` as `[Type]` in the definition of `HList`
import Data.Kind (Type)

five :: Int
five = 5

five_ :: (a ~ Int) => a
five_ = 5

data Expr a where
  LitInt  :: Int -> Expr Int
  LitBool :: Bool -> Expr Bool
  Add     :: Expr Int -> Expr Int -> Expr Int
  Not     :: Expr Bool -> Expr Bool
  If      :: Expr Bool -> Expr a -> Expr a -> Expr a

deriving instance Show a => Show (Expr a)

evalExpr :: Expr a -> a
evalExpr (LitInt i) = i
evalExpr (LitBool b) = b
evalExpr (Add x y) = evalExpr x + evalExpr y
evalExpr (Not x) = not $ evalExpr x
evalExpr (If b x y) =
  if evalExpr b
    then evalExpr x
    else evalExpr y


data Expr_ a
  = (a ~ Int)    => LitInt_ Int
  | (a ~ Bool)   => LitBool_ Bool
  | (a ~ Int)    => Add_ (Expr_ Int) (Expr_ Int)
  | (a ~ Bool)   => Not_ (Expr_ Bool)
  | If_ (Expr_ Bool) (Expr_ a) (Expr_ a)


data HList (ts :: [Type]) where
  HNil :: HList '[]
  (:#) :: t -> HList ts -> HList (t ': ts)

infixr 5 :#

hLength :: HList ts -> Int
hLength HNil      = 0
hLength (_ :# ts) = 1 + hLength ts

hHead :: HList (t ': ts) -> t
hHead (t :# _) = t

showBool :: HList '[_1, Bool, _2] -> String
showBool (_ :# b :# _ :# HNil) = show b

instance Eq (HList '[]) where
  HNil == HNil = True

instance (Eq t, Eq (HList ts)) => Eq (HList (t ': ts)) where
  (a :# as) == (b :# bs) = a == b && as == bs