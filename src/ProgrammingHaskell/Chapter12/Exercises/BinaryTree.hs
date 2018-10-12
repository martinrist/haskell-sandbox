module ProgrammingHaskell.Chapter12.Exercises.BinaryTree where

data BinaryTree a
    = Leaf
    | Node (BinaryTree a)
           a
           (BinaryTree a)
    deriving (Eq, Ord, Show)

-- Question 1
unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f a =
    case f a of
        Nothing -> Leaf
        Just (a1, b, a2) -> Node (unfold f a1) b (unfold f a2)

-- Question 2
treeBuild :: Integer -> BinaryTree Integer
treeBuild n =
    unfold
        (\a ->
             if a == n
                 then Nothing
                 else Just (a + 1, a, a + 1))
        0