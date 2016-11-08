uncurriedAdd :: Num a => (a, a) -> a
uncurriedAdd (x, y) = x + y

h :: (Num a, Num b) => a -> b -> b
h = undefined

jackal :: (Ord a, Eq b) => a -> b -> a
jackal = undefined
