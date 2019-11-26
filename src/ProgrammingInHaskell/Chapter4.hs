module ProgrammingInHaskell.Chapter4 where

-- | Calculate the absolute value of n
abs :: Integral a => a -> a
abs n | n >= 0    = n
      | otherwise = -n

-- | Calculate the signum value of n
signum :: Integral a => a -> a
signum n | n < 0     = -1
         | n == 0    = 0
         | otherwise = 1

-- Exercise 4.1 - `halve`

-- | Split a list into two halves
halve :: [a] -> ([a], [a])
halve xs = (take n xs, drop n xs) where n = length xs `div` 2


-- Exercise 4.2 - `third`

-- | Return the third element in the list, using `head` and `tail`:
thirdUsingHeadAndTail :: [a] -> a
thirdUsingHeadAndTail = head . tail . tail

-- | Return the third element in the list, using `!!`:
thirdUsingListIndexing :: [a] -> a
thirdUsingListIndexing xs = xs !! 2

-- | Return the third element in the list, using pattern matching
thirdUsingPatternMatching :: [a] -> a
thirdUsingPatternMatching (_ : _ : x : _) = x
thirdUsingPatternMatching _ = undefined


-- Exercise 4.3 - `safetail`

-- | Return the tail of the list, or [] if empty, using conditionals
safetailUsingConditionals :: [a] -> [a]
safetailUsingConditionals xs = if null xs then [] else tail xs

-- | Return the tail of the list, or [] if empty, using guards
safetailUsingGuards :: [a] -> [a]
safetailUsingGuards xs | null xs   = []
                       | otherwise = tail xs

-- | Return the tail of the list, or [] if empty, using pattern matching
safetailUsingPatternMatching :: [a] -> [a]
safetailUsingPatternMatching []       = []
safetailUsingPatternMatching (_ : xs) = xs


-- Exercise 4.4 - Disjunction using pattern matching

(||) :: Bool -> Bool -> Bool
False || False = False
_     || _     = True
