{-# OPTIONS_GHC -Wall #-}

module LibraryFunctions where
  
import Data.Monoid
  
sum' :: (Foldable t, Num a) => t a -> a
--sum' = foldr (+) 0
sum' x = getSum $ (foldMap Sum) x

product' :: (Foldable t, Num a) => t a -> a
--product' = foldr (*) 1
product' = getProduct . foldMap Product

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
--elem' x = foldr ((||) . (==) x) False
elem' x  = getAny . foldMap (Any . (== x))

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = undefined

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = undefined

null' :: (Foldable t) => t a -> Bool
null' a = length' a == 0

length' :: (Foldable t) => t a -> Int
length' = foldr (\_ i -> i + 1) 0

toList' :: (Foldable t) => t a -> [a]
toList' = foldr (:) []

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (mappend . f) mempty