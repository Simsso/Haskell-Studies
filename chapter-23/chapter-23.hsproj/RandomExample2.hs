{-# OPTIONS_GHC -Wall #-}

module RandomExample2 where
  
import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import System.Random
import RandomExample

rollDie :: State StdGen Die
rollDie = state $ do
  (n, s) <- randomR (1, 6)
  return (intToDie n, s)
  
rollDie' :: State StdGen Die
rollDie' = intToDie <$> state (randomR (1, 6))

rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' = liftA3 (,,) rollDie rollDie rollDie

infiniteDie :: State StdGen [Die]
infiniteDie = repeat <$> rollDie

nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie


-- Roll Your Own (PDF page 935)
rollsToGetTwenty :: Int -> StdGen -> Int
rollsToGetTwenty n g = go 0 0 g where
  go :: Int -> Int -> StdGen -> Int
  go sums count gen
    | sums >= n = count
    | otherwise =
      let (die, nextGen) = randomR (1 :: Int, 6) gen
      in go (sums + die) (count + 1) nextGen
      
rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n g = go 0 (0, []) g where
  go :: Int -> (Int, [Die]) -> StdGen -> (Int, [Die])
  go sums count gen
    | sums >= n = count
    | otherwise =
      let (die, nextGen) = randomR (1 :: Int, 6) gen  
          newDieHistory  = intToDie die : snd count
          newDieCount    = fst count + 1
      in go (sums + die) (newDieCount, newDieHistory) nextGen