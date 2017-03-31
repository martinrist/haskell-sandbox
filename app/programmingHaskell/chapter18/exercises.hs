import Control.Monad (join)

andOne :: Int -> [Int]
andOne x = [x, 1]

-- Writing `bind` in terms of `fmap` and `join`

bind :: Monad m => (a -> m b) -> m a -> m b
bind f x = join $ fmap f x