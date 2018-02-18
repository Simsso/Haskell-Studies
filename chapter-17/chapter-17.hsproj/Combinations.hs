{-# OPTIONS_GHC -Wall #-}

module Combinations where
  
-- Write a function that takes inputs from stops and vowels
-- and makes 3-tuples of all possible stop-vowel-stop combinations.

import Control.Applicative (liftA3)

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)