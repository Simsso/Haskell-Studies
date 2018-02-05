{-# LANGUAGE DeriveGeneric #-}

module Test.SimpleArithmeticTest where
  
import Test.Hspec
import Test.QuickCheck

import Data.List(sort)

import SimpleArithmetic

-- list generator
listOf :: Gen a -> Gen [a]
listOf gen = sized $ \n ->
  do k <- choose (0,n)
     vectorOf k gen

--coarbitrary :: CoArbitrary a => a -> Gen b -> Gen b
--coarbitrary arbitrary arbitrary

main :: IO ()
main = hspec $ do
  describe "Half" $ do
    it "half x * .2 == x / 2" $ do
      property $ (\x -> halfIdentity (x :: Rational) == x)
      
  describe "Sort" $ do
    it "sorts an array" $ do
      property $ (\xs -> (listOrdered . sort) (xs :: [Integer]) == True) 
      
  describe "Associative" $ do
    it "Addition" $ do
      property $ (\x -> \y -> \z -> plusAssociative x y z)
    it "Multiplication" $ do
      property $ (\x -> \y -> \z -> multAssociative x y z)
    it "Power" $ do
      property $ (\x -> \y -> \z -> powAssociative x y z)
      
  describe "Commutative" $ do
    it "Addition" $ do
      property $ (\x -> \y -> plusCommutative x y)
    it "Multiplication" $ do
      property $ (\x -> \y -> multCommutative x y)
    it "Power" $ do
      property $ (\x -> \y -> powCommutative x y)
      
  describe "QuotRem" $ do
    it "(quot x y)*y + (rem x y) == x" $ do
      property $ (\x -> \y -> quotRem1 x (y :: Integer))
    it "(div x y)*y + (mod x y) == x" $ do
      property $ (\x -> \y -> quotRem2 x (y :: Integer))
      
  describe "Reverse id" $ do
    it "reverse . reverse == id" $ do
      property $ (\x -> (reverse . reverse) [x] == id ([x] :: [Integer]))
      
  describe "$" $ do
    it "$" $ do
      property $ (\a f -> f $ a == f a)
      
  describe "Foldr (:) and (++)" $ do
    it "Are equal" $ do
      property $ (\x -> \y -> foldr (:) y x == (++) y x)
      
  describe "read and show" $ do
    it "Cancel each other out" $ do
      property $ (\x -> (read (show x)) == x)
