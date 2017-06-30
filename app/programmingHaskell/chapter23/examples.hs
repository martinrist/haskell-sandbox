import           Control.Applicative       (liftA3)
import           Control.Monad             (replicateM)
import           Control.Monad.Trans.State
import           System.Random

-----------------------------------
-- Chapter 23 - State - Examples --
-----------------------------------
main :: IO ()
main = undefined



-- 23.5 - Throw Down
--------------------

data Die =
      DieOne
    | DieTwo
    | DieThree
    | DieFour
    | DieFive
    | DieSix
    deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
    case n of
         1 -> DieOne
         2 -> DieTwo
         3 -> DieThree
         4 -> DieFour
         5 -> DieFive
         6 -> DieSix
         x -> error $ "intToDie got non-1-6 integer: " ++ show x

roll3Dice :: (Die, Die, Die)
roll3Dice = do
    let s = mkStdGen 0
        (d1, s1) = randomR (1, 6) s
        (d2, s2) = randomR (1, 6) s1
        (d3, s3) = randomR (1, 6) s2
    (intToDie d1, intToDie d2, intToDie d3)

rollDie :: State StdGen Die
rollDie = state $ do
    (n, s) <- randomR (1, 6)
    return (intToDie n, s)

-- Alternatively, lift `intToDie` over the result of `state`
rollDie' :: State StdGen Die
rollDie' = intToDie <$> state (randomR (1, 6))

-- Trying to use repeat to roll a list doesn't work
infiniteDie :: State StdGen [Die]
infiniteDie = repeat <$> rollDie

-- We need to use `replicateM` to carry the state along
nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie
