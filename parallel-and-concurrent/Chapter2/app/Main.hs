module Main where

import Data.Time.Clock (getCurrentTime)
import Lib (runSlowParallel, printTimeSince, fizzleSample)
import Control.Parallel.Strategies (Eval, rpar, rseq, runEval)

main :: IO ()
main = do
    print fizzleSample
    t0 <- getCurrentTime
    printTimeSince t0
    fibResult <- runSlowParallel
    printTimeSince t0
    print fibResult
    printTimeSince t0
