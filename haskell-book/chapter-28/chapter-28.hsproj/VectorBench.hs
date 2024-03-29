module VectorBench where

import Criterion.Main
import qualified Data.Vector as V

slice' :: Int -> Int -> [a] -> [a]
slice' from len = take len . drop from

slice'' :: Int -> Int -> [a] -> [a]
slice'' = flip $ flip (.) ((.)(.)(.) (flip take) drop) . flip flip

slice :: Int -> Int -> [a] -> [a]slice from len xs = take len (drop from xs)

l :: [Int]
l = [1..1000]

v :: V.Vector Int
v = V.fromList [1..1000]

main :: IO ()
main = defaultMain
  [ bench "slicing list" $
    whnf (head . slice 100 900) l
  , bench "slicing list a little point-free" $
    whnf (head . slice' 100 900) l
  , bench "slicing list very point-free" $
    whnf (head . slice'' 100 900) l
  , bench "slicing vector" $
    whnf (V.head . V.slice 100 900) v
  ]