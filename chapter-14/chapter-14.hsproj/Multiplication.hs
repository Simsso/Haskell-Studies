module Multiplication where
  
multiply :: (Eq a, Num a) => a -> a -> a
multiply m1 m2 = if signum m1 == -1
  then -go (-m1) 
  else go m1 
  where
    go 0   = 0
    go m1' = m2 + go (m1' - 1)