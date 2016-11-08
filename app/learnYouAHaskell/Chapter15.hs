module Chapter15 where

-- Define the `Tree` data structure
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

-- And a simple tree that we can play with
freeTree :: Tree Char
freeTree =
   Node 'P'
        (Node 'O'
            (Node 'L'
                (Node 'N' Empty Empty)
                (Node 'T' Empty Empty)
            )
            (Node 'Y'
                (Node 'S' Empty Empty)
                (Node 'A' Empty Empty)
            )
        )
        (Node 'L'
            (Node 'W'
                (Node 'C' Empty Empty)
                (Node 'R' Empty Empty)
            )
            (Node 'A'
                (Node 'A' Empty Empty)
                (Node 'C' Empty Empty)
            )
        )

-- Nasty way of changing the 'W' to a 'P' is as follows:
nastyChangeToP :: Tree Char -> Tree Char
nastyChangeToP (Node x l (Node y (Node _ m n) r)) = Node x l (Node y (Node 'P' m n) r)


-- A better way is to have a function that takes a tree and a list of directions

data Direction = L | R deriving (Show)
type Directions = [Direction]

changeToP :: Directions -> Tree Char -> Tree Char
changeToP (L:ds) (Node x l r) = Node x (changeToP ds l) r
changeToP (R:ds) (Node x l r) = Node x l (changeToP ds r)
changeToP [] (Node _ l r) = Node 'P' l r


-- To make it easier, create a function that prints out the element at a position
-- given by a list of directions
elemAt :: Directions -> Tree a -> a
elemAt (L:ds) (Node _ l _) = elemAt ds l
elemAt (R:ds) (Node _ _ r) = elemAt ds r
elemAt [] (Node x _ _) = x


-- This is, however, quite inefficient if we need to address a lot of elements far
-- down in the tree, because we need to start from the top each time.

-- Instead use a trail of breadcrumbs as we head down the tree
type Breadcrumbs = [Direction]

-- Now we can go left and right down the tree, accumulating breadcrumbs as we go.
-- Note that the breadcrumbs are in reverse order, so the first breadcrumb is last
-- in the list.
goLeft' :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs)
goLeft' (Node _ l _, bs) = (l, L:bs)

goRight' :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs)
goRight' (Node _ _ r, bs) = (r, R:bs)


-- This helper function allows us to do something like:
-- (freeTree, []) -: goLeft -: goRight
(-:) :: t1 -> (t1 -> t) -> t
x -: f = f x


-- As it is, the breadcrumbs don't tell us enough to go back up the tree,
-- since it doesn't have information about the path not chosen.

-- To do this, we define a more complex version of `Direction`,
-- where `L` is replaced by `LeftCrumb`, which contains the element in the
-- node we moved from, and the right tree we didn't visit.
data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show)
type Crumbs a = [Crumb a]

-- A pair that contains a focused part of a datastructure and its surroundings
-- is called a zipper
type TreeZipper a = (Tree a, Crumbs a)

-- Now `goLeft` and `goRight` work with `Crumbs` that also support an ability to
-- `goUp`
goLeft :: TreeZipper a -> Maybe (TreeZipper a)
goLeft (Node x l r, bs) = Just (l, LeftCrumb x r:bs)
goLeft (Empty, _) = Nothing

goRight :: TreeZipper a -> Maybe (TreeZipper a)
goRight (Node x l r, bs) = Just (r, RightCrumb x l:bs)
goRight (Empty, _) = Nothing

goUp :: TreeZipper a -> Maybe (TreeZipper a)
goUp (t, LeftCrumb x r:bs) = Just (Node x t r, bs)
goUp (t, RightCrumb x l:bs) = Just (Node x l t, bs)
goUp (_, []) = Nothing



-- Now we can write `modify` that applies a function to the element in the
-- root of the subtree on which the zipper is focussing:
modify :: (a -> a) -> TreeZipper a -> TreeZipper a
modify f (Node x l r, bs) = (Node (f x) l r, bs)
modify _ (Empty, bs) = (Empty, bs)


-- Going all the way to the top is also easy
topMost :: TreeZipper a -> Maybe (TreeZipper a)
topMost (t, []) = Just (t, [])
topMost z = goUp z >>= topMost