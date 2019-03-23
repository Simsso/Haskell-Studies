module Lib
    ( runSlowParallel,
      printTimeSince,
      fizzleSample
    ) where

import Control.Parallel ()
import Control.Parallel.Strategies (Eval, rpar, rseq, runEval)
import Control.Exception (evaluate)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Text.Printf (printf)
import System.Environment ()

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)


printTimeSince t0 = do
    t1 <- getCurrentTime
    printf "time: %.2fs\n" (realToFrac (diffUTCTime t1 t0) :: Double)

runSlowParallel :: IO (Integer, Integer)
runSlowParallel = evaluate $ runEval slowParallel

slowParallel :: Eval (Integer, Integer)
slowParallel = do
    a <- rpar $ fib 40
    b <- rpar $ fib 41
    rseq a
    --rseq b
    return (a,b)

fizzleSample = runEval $ do
    let a = fib 40
    b <- rpar (a + fib 41)
    c <- rpar a
    rseq b
    return (b,c)
