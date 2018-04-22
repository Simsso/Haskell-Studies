{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE InstanceSigs #-}

module Bifunctor where

class Bifunctor p where
  {-# MINIMAL bimap | first, second #-}
  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f g = first f . second g

  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id

  second :: (b -> c) -> p a b -> p a c
  second = bimap id
  
data Deux a b = Deux a b
instance Bifunctor Deux where
  first  ab (Deux a b) = Deux (ab a) b
  second bc (Deux a b) = Deux a $ bc b
  bimap  ab bc (Deux a b) = Deux (ab a) (bc b)

data Const a b = Const a
instance Bifunctor Const where
  bimap ab _ (Const a) = Const $ ab a

data Drei a b c = Drei a b c
instance Bifunctor (Drei a) where
  bimap ab bc (Drei c a b) = Drei c (ab a) (bc b)

data SuperDrei a b c = SuperDrei a b
instance Bifunctor (SuperDrei a) where
  bimap ab _ (SuperDrei c a) = SuperDrei c $ ab a

data SemiDrei a b c = SemiDrei a
instance Bifunctor (SemiDrei a) where
  bimap _ _ (SemiDrei c) = (SemiDrei c)

data Quadriceps a b c d = Quadriceps a b c d
instance Bifunctor (Quadriceps a b) where
  bimap ab bc (Quadriceps c d a b) = Quadriceps c d (ab a) (bc b)

data Either' a b = Left' a | Right' b
instance Bifunctor Either' where
  bimap ab _ (Left' a)  = Left' $ ab a
  bimap _ bc (Right' b) = Right' $ bc b