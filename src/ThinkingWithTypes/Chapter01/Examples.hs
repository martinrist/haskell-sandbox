module ThinkingWithTypes.Chapter01.Examples where

import           Data.Void

prodUnitTo :: a -> (a, ())
prodUnitTo a = (a, ())

prodUnitFrom :: (a, ()) -> a
prodUnitFrom (a, ()) = a

sumUnitTo :: Either a Void -> a
sumUnitTo (Left  a) = a
sumUnitTo (Right v) = absurd v

sumUnitFrom :: a -> Either a Void
sumUnitFrom = Left

data TicTacToe a = TicTacToe
    { topLeft :: a
    , topCentre :: a
    , topRight :: a
    , midLeft :: a
    , midCentre :: a
    , midRight :: a
    , bottomLeft :: a
    , bottomCentre :: a
    , bottomRight :: a
    }

emptyBoard :: TicTacToe (Maybe Bool)
emptyBoard =
    TicTacToe
        Nothing Nothing Nothing
        Nothing Nothing Nothing
        Nothing Nothing Nothing

data Three = One | Two | Three

data TicTacToe2 a = TicTacToe2
    { board :: Three -> Three -> a
    }

emptyBoard2 :: TicTacToe2 (Maybe Bool)
emptyBoard2 =
    TicTacToe2 $ const $ const $ Nothing