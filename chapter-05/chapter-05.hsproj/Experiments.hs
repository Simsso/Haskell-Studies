module Experiments where
import Data.Dynamic
f :: Int -> Int
f x = x + 5


sub :: Num a => a -> a -> a
sub x y = x - y

-- partial application
subTuple = uncurry sub

-- uncurry function with three parameters
add :: Num a => a -> a -> a -> a
add x y z = x + y + z

addTupleVal = uncurry add
addTuple = uncurry addTupleVal