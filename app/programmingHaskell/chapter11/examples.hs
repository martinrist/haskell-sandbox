
------------------------------------
-- 11.3 - Data and type constructors
------------------------------------

-- Nullary constructors, or constants
data Trivial = Trivial'

-- Unary constructors
data UnaryTypeCon a = UnaryValueCon a

-- Binary constructors
data BinaryTypeCon a b = BinaryValueCon a b


--------------------------------------
-- 11.5 - Data constructors and values
--------------------------------------

-- PugData is a constant value
data PugType = PugData

-- The type variable argument 'a' does not appear as an argument to the data constructor, so it is a 'phantom'
data HuskyType a = HuskyData



data Doggies a =
      Husky a
    | Mastiff a
    deriving (Eq, Show)
