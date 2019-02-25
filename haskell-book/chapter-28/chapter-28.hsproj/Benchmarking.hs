module Benchmarking where
  
import Criterion.Main

infixl 9 !?

_ !? n | n < 0 = Nothing
[] !? _ = Nothing
(x:_) !? 0 = Just x
(_:xs) !? n = xs !? (n-1)

main :: IO ()
main = defaultMain
  [ bench "index list 9999" $ whnf ([1..9999] !!) 9998 ]