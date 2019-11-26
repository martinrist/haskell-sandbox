{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}

module ThinkingWithTypes.Chapter04.Examples where

import           Data.Typeable

broken :: (a -> b) -> a -> b
broken f a = apply
    where
        -- This fails to compile if we remove the type annotation comment
        -- apply :: b
          apply = f a


-- The explicit `forall`, along with `ScopedTypeVariables` makes this work
working :: forall a b . (a -> b) -> a -> b
working f a = apply
  where
    apply :: b
    apply = f a

typeName :: forall a. Typeable a => String
typeName = show . typeRep $ Proxy @a


type family AlwaysUnit a where
    AlwaysUnit a = ()

-- This type signature isn't ambiguous
func1 :: AlwaysUnit a -> a
func1 = undefined

-- This type signature isn't ambiguous
func2 :: b -> AlwaysUnit a -> b
func2 = undefined

-- This type signature *is* ambiguous
func3 :: Show a => AlwaysUnit a -> String
func3 = undefined