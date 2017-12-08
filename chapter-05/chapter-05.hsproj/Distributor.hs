-- source: Dominik Müller

module Distributor where

distributor :: (a -> b -> c) -> (a -> b) -> (a -> c)
distributor f s i = f i (s i)

ifThenElse :: Bool -> a -> a -> a
ifThenElse c x y = if c then x else y

extender :: a -> b -> a
extender f i = f

isEven :: Int -> Bool
isEven n = even n

half :: Int -> Int
half n = div n 2



threeTimesPlusOne :: Int -> Int
threeTimesPlusOne n = 3 * n + 1

c1 = extender ifThenElse
c2 = distributor c1
c3 = c2 isEven
c4 = distributor c3
c5 = c4 half
c6 = distributor c5
c7 = c6 threeTimesPlusOne

example = c7