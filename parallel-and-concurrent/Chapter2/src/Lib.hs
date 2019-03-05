module Lib
    ( runSlowParallel,
      printTimeSince
    ) where

import Control.Parallel ()
import Control.Parallel.Strategies (Eval, rpar, rseq, runEval)
import Control.Exception (evaluate)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Text.Printf (printf)
import System.Environment ()

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)


printTimeSince t0 = do
    t1 <- getCurrentTime
    printf "time: %.2fs\n" (realToFrac (diffUTCTime t1 t0) :: Double)

runSlowParallel :: IO (Int, Int)
runSlowParallel = evaluate $ runEval slowParallel

slowParallel :: Eval (Int, Int)
slowParallel = do
    a <- rpar $ fib 42
    b <- rpar $ fib 43
    rseq a
    --rseq b
    return (a,b)
