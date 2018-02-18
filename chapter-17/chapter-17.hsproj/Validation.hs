{-# OPTIONS_GHC -Wall #-}

module Validation where
  
data Validation e a = Failure e | Success a
  deriving (Eq, Show)

-- same as Either
instance Functor (Validation e) where
  fmap _ (Failure e) = Failure e
  fmap f (Success a) = Success $ f a

-- This is different
instance Monoid e => Applicative (Validation e) where
  pure = Success
  (<*>) (Failure e1) (Failure e2) = Failure $ mappend e1 e2
  (<*>) (Failure e) _ = Failure e
  (<*>) _ (Failure e) = Failure e
  (<*>) (Success f) (Success a) = Success $ f a