module ProgrammingHaskell.Chapter15.Exercises.Optional where

data Optional a =
    Nada
    | Only a
    deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
    Nada <> Nada = Nada
    Only a <> Nada = Only a
    Nada <> Only b = Only b
    Only a <> Only b = Only $ a <> b

instance Monoid a => Monoid (Optional a) where
    mempty = Nada
