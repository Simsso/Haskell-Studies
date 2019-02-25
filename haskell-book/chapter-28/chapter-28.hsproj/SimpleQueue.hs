{-# OPTIONS_GHC -Wall #-}

module SimpleQueue where
  
---import Criterion.Main

-- From Okasaki's Purely
-- Functional Data Structures
data Queue a = Queue {
    enqueue :: [a]
  , dequeue :: [a]
} deriving (Eq, Show)

-- adds an item
push :: a -> Queue a -> Queue a
push = undefined

pop :: Queue a -> Maybe (a, Queue a)
pop = undefined


type Queue_ a = [a]

push_ :: a -> Queue_ a -> Queue_ a
push_ x xs = x:xs

pop_ :: Queue_ a -> Maybe (a, Queue_ a)
pop_ [] = Nothing
pop_ queue_ = go [] queue_ where
  go xs (q:[]) = Just (q, xs)
  go xs (q:qs) = undefined
 