{-# OPTIONS_GHC -Wall #-}

-- http://haskell.1045720.n5.nabble.com/Threads-and-hGetLine-td5672946.html#a5681339
-- https://gist.github.com/erantapaa/ebbcd56d1bccf3e57c75

module ExitIfIdle where
  
import System.IO

timeout :: Int
timeout = 2000
  
main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  eventl
  putStrLn "exiting"
  
eventl :: IO ()
eventl = do
  timedout <- hWaitForInput stdin timeout
  s <- hGetLine stdin
  if length s == 0 || timedout
    then return ()
    else eventl