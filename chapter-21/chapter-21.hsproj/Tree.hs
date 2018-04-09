{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Tree where
  
import Test.QuickCheck
import Test.QuickCheck.Checkers

import Data.Monoid
  

data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a)
  deriving (Eq, Show)
  
newtype PreOrderTree a = PreOrderTree { baseTree :: Tree a }

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf a) = Leaf $ f a
  fmap f (Node t1 a t2) = Node (fmap f t1) (f a) (fmap f t2)

instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf a) = f a
  foldMap f (Node t1 a t2) = foldMap f t1 <> f a <> foldMap f t2

-- in order
instance Traversable Tree where
  traverse _ Empty = pure Empty
  traverse f (Leaf a) = fmap Leaf $ f a
  traverse f (Node t1 a t2) = Node <$> traverse f t1 <*> f a <*> traverse f t2
  
instance (Arbitrary a) => Arbitrary (Tree a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    elements [
      (Empty),
      (Leaf b),
      (Node a b c)]
      
instance (Eq a) => EqProp (Tree a) where
  (=-=) = eq


data AllTrees a = InOrder (Tree a) | PostOrder (Tree a)

--instance Functor AllTrees where
