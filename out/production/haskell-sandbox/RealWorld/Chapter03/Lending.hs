module RealWorld.Chapter03.Lending where

lend :: Int -> Int -> Maybe Int
lend amount balance = let reserve = 100
                          newBalance = balance - amount
                      in if balance < reserve
                            then Nothing
                            else Just newBalance

lend2 :: Double -> Double -> Maybe Double
lend2 amount balance = if amount < reserve * 0.5
                          then Just newBalance
                          else Nothing
                        where reserve    = 100
                              newBalance = balance - amount

-- A different version, using guards

lend3 :: Double -> Double -> Maybe Double
lend3 amount balance
    | amount <= 0 = Nothing
    | amount > reserve * 0.5 = Nothing
    | otherwise = Just newBalance
    where reserve = 100
          newBalance = balance - amount
