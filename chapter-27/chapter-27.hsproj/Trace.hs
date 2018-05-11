{-# OPTIONS_GHC -Wall #-}

module Trace where
  
import Debug.Trace

f :: Num a => a -> a
f = trace "f called" (+) 1

f' :: Int -> Int
f' b = trace "f' called" 1 + b

f'' :: (() -> Int) -> Int
f'' x = (x ()) + (x ())