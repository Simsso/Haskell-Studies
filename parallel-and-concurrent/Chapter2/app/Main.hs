module Main where

import Data.Time.Clock (getCurrentTime)
import Lib (runSlowParallel, printTimeSince)

main :: IO ()
main = do
    t0 <- getCurrentTime
    printTimeSince t0
    fibResult <- runSlowParallel
    printTimeSince t0
    print fibResult
    printTimeSince t0
