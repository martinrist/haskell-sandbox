-- Extensions required for the definition of `HList`:
{-# LANGUAGE GADTs #-}

-- This allows us to start talking about kinds other than just `Type`,
-- `Constraint` and `->`
{-# LANGUAGE DataKinds #-}

-- This allows us to give `ts` a kind signature in the definition of `HList`
-- It's actually not needed here, because it's implied by `TypeFamilies`
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# LANGUAGE KindSignatures #-}

-- This allows us to use `':` in the definition of `HList`
{-# LANGUAGE TypeOperators #-}

-- This is needed to write the closed type family `AllEq`
{-# LANGUAGE TypeFamilies #-}

-- This is needed to write the nested constraint `All Eq ts`
{-# LANGUAGE UndecidableInstances #-}


module ThinkingWithTypes.Chapter05.Examples2 where

-- This allows us to refer to the kind of `ts` as `[Type]` in the definition
-- of `HList`
import Data.Kind (Type, Constraint)


-- Here, `ts` has an explicit kind signature.  Adding this is a good idea if
-- it's anything other than `*`.  This needs `DataKinds` and `KindSignatures`.
data HList (ts :: [Type]) where
  HNil :: HList '[]
  -- This needs `TypeOperators` in order to use the `':` type cons operator
  -- Symbolically-named data constructors must start with a `:`
  (:#) :: t -> HList ts -> HList (t ': ts)

infixr 5 :#


-- These need `TypeFamilies`:

type family AllEq (ts :: [Type]) :: Constraint where
  AllEq '[]       = ()
  AllEq (t ': ts) = (Eq t, AllEq ts)

type family All (c :: Type -> Constraint)
                (ts :: [Type]) :: Constraint where
  All c '[]       = ()
  All c (t ': ts) = (c t, All c ts)


-- This needs `UndecidableInstances`:

instance All Eq ts => Eq (HList ts) where
  HNil      == HNil      = True
  (a :# as) == (b :# bs) = a == b && as == bs