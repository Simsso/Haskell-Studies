{-# OPTIONS_GHC -Wall #-}

module SOQuestion where

newtype Constant a b = Constant { getConstant :: a }
  deriving (Eq, Show)
  
--instance Functor (Constant a) where
--  fmap _ (Constant x) = Constant x

instance Functor (Constant a) where
  fmap _ x = x
