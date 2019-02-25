{-# OPTIONS_GHC -Wall #-}

import Data.Char
import System.Environment
import System.IO

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
      c <- hGetChar stdin
      hPutStr stdout [conversion c]
      loop conversion
    exit :: IO ()
    exit = putStrLn "Pass either -u or -l as argument"
    argcheck :: [String] -> Bool
    argcheck args =
      length args /= 0 && (
        args !! 0 == "-u" ||
        args !! 0 == "-l")
