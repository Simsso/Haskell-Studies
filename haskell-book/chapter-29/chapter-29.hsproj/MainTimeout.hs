{-# OPTIONS_GHC -Wall #-}

module MainTimeout where
  
import Data.Char
import System.Environment
import System.IO


-- https://ghc.haskell.org/trac/ghc/ticket/8684

main :: IO ()
main = do
  args <- getArgs
  putStrLn "args: "
  putStrLn $ show args
  if argcheck args 
    then loop (if args !! 0 == "-u" then toUpper else toLower) 
    else exit
  where
    loop conversion = do
      timeout <- hWaitForInput stdin 5000
      if not timeout then do
        c <- hGetChar stdin
        hPutStr stdout [conversion c]
        loop conversion
        else hPutStrLn stderr "Timeout"
    exit :: IO ()
    exit = putStrLn "Pass either -u or -l as argument"
    argcheck :: [String] -> Bool
    argcheck args =
      length args /= 0 && (
        args !! 0 == "-u" ||
        args !! 0 == "-l")
