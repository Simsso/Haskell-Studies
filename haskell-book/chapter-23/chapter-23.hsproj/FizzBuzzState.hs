module FizzBuzzState where

import Control.Monad
import Control.Monad.Trans.State

fizzBuzz :: Integer -> String
fizzBuzz n
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod` 5 == 0 = "Buzz"
  | n `mod` 3 == 0 = "Fizz"
  | otherwise = show n
  
fizzbuzzList :: [Integer] -> [String]
fizzbuzzList list = execState (mapM_ addResult list) []

addResult :: Integer -> State [String] ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (result : xs)
  
fizzbuzzFromTo :: Integer -> Integer -> [String]
fizzbuzzFromTo a b = fizzbuzzList $ enumFromThenTo b (b-1) a
  
main :: IO ()
--main = mapM_ putStrLn $ reverse $ fizzbuzzList [1..100]
main = mapM_ putStrLn $ fizzbuzzFromTo 1 100