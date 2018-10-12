-----------------------
-- Chapter Exercises --
-----------------------
-- Unfolds
----------
-- Question 1 - myIterate using direct recursion
myIterate :: (a -> a) -> a -> [a]
myIterate f z = z : myIterate f (f z)

-- Question 2- custom myUnfoldr using direct recusion
myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b =
    case f b of
        Nothing -> []
        Just (a, b) -> a : myUnfoldr f b

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\b -> Just (b, f b))

-- Binary tree
--------------
data BinaryTree a
    = Leaf
    | Node (BinaryTree a)
           a
           (BinaryTree a)
    deriving (Eq, Ord, Show)

-- Question 1 - unfold for Binary Tree
unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f a =
    case f a of
        Nothing -> Leaf
        Just (a1, b, a2) -> Node (unfold f a1) b (unfold f a2)

-- Question 2 - Tree builder
treeBuild :: Integer -> BinaryTree Integer
treeBuild n =
    unfold
        (\b ->
             if b == n
                 then Nothing
                 else Just (b + 1, b, b + 1))
        0
