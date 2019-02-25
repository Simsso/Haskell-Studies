module SimpleArithmetic where

import Data.List (sort)

  
half x = x / 2


halfIdentity = (*2) . half


listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t)      = (Just y, t)
        go y (Just x, t)       = (Just y, x >= y)
        

associative :: Eq a => (a -> a -> a) -> a -> a -> a -> Bool
associative op x y z = x `op` (y `op` z) == (x `op` y) `op` z

commutative :: Eq a => (a -> a -> a) -> a -> a -> Bool
commutative op x y = x `op` y == y `op` x


plusAssociative = associative (+)
plusCommutative = commutative (+)


multAssociative = associative (*)
multCommutative = commutative (*)

quotRem1 x y = (quot x y) * y + (rem x y) == x
quotRem2 x y = (div x y) * y + (mod x y) == x


powAssociative = associative (^)
powCommutative = commutative (^)