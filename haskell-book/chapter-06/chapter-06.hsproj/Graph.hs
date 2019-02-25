module Graph where

-- inspired by https://gist.github.com/ethylamide/2793684
-- binary tree
data BinTree a = Empty | Leaf a | Node a (BinTree a) (BinTree a)

instance (Show a) => Show (BinTree a) where
  show Empty = "(-)"
  show (Leaf a) = "leaf(" ++ (show a) ++ ")"
  show (Node a b c) = "\n" ++ show a ++ "\n" ++ (show b) ++ " " ++ (show c)
