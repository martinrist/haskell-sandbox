---------------------------
-- Chapter 17 - Examples --
---------------------------

-- 17.5 - Applicative in use

newtype Identity a = Identity a
    deriving (Eq, Ord, Show)

newtype Constant a b =
    Constant { getConstant :: a }
    deriving (Eq, Ord, Show)
