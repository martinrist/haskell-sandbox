import           Control.Monad.Trans.State hiding (get, modify, put)

------------------------------------
-- Chapter 23 - Chapter Exercises --
------------------------------------
main :: IO ()
main = undefined

-- Exercise 1 - get
get :: State s s
get = state $ \s -> (s, s)


-- Exercise 2 - put
put :: s -> State s ()
put s = state $ const ((), s)


-- Exercise 3 - exec
exec :: State s a -> s -> s
exec statesa s = snd $ runState statesa s


-- Exercise 4 - eval
eval :: State s a -> s -> a
eval statesa s = fst $ runState statesa s

-- Exercise 5 - modify
modify :: (s -> s) -> State s ()
modify f = state $ \s -> ((), f s)
