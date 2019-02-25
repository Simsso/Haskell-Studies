{-# OPTIONS_GHC -Wall #-}

module Pop where

import Criterion.Main

type Queue_ a = [a]

pop_ :: Queue_ a -> Maybe (a, Queue_ a)
pop_ queue_ = case reverse queue_ of
  x:xs -> Just (x, reverse xs)
  []   -> Nothing


type Queue a = [a]

type PopFn a = Queue a -> Maybe (a, Queue a)

push :: a -> Queue a -> Queue a
push x xs = x:xs

pop :: Queue_ a -> Maybe (a, Queue_ a)
pop queue_ = case reverse queue_ of
  x:xs -> Just (x, reverse xs)
  []   -> Nothing

pop1 :: Queue a -> Maybe (a, Queue a)
pop1 []     = Nothing
pop1 (x:xs) = Just (go x xs)
  where go a []     = (a, [])
        go a (y:ys) = let (z, zs) = go y ys
                      in (z, a:zs)
                      
pop2 :: Queue a -> Maybe (a, Queue a)
pop2 [] = Nothing
pop2 xs = let n = length xs
              e = drop (n-1) xs !! 0
              s = take (n-1) xs
          in  Just(e, s)
          
                      
pop2' :: Queue a -> Maybe (a, Queue a)
pop2' [] = Nothing
pop2' xs = let n = length xs
               (es,ss) = splitAt (n-1) xs
           in  Just(ss !! 0, es)
                      
pop3 :: Queue Int -> Maybe (Int, Queue Int)
pop3 [] = Nothing
pop3 xs = let   nm1 :: Int
                nm1  = length xs - 1
                getx = (!!) xs :: Int -> Int
                go :: Int -> [Int] -> [Int]
                go 0 l = l
                go i l = go (i-1) ((getx i) : l)
          in    Just (getx nm1, go nm1 [])
          
pop4 :: Queue Int -> Maybe (Int, Queue Int)
pop4 xs = (,) (xs !! length xs - 1) <$> (foldr ((.) Just . maybe [] . (:)) Nothing) xs


test :: PopFn Int -> Int -> Queue_ Int
test f i = go i [1..100] where
  go 0 queue_ = queue_
  go n queue_ = case f (push 69 queue_) of
    Just (_, q_) -> go (n-1) q_
    Nothing      -> error "error"

main :: IO ()
main =
  let nftype = nf
  in
    defaultMain
  [ bench "pop double reverse" $
    nftype (test pop_) 12345
  , bench "Dominik's pop" $
    nftype (test pop1) 12345
  , bench "Timo's pop (drop/take)" $
    nftype (test pop2) 12345
  , bench "Out splitAt" $
    nftype (test pop2') 12345
  , bench "Timo's pop (cache?!)" $
    nftype (test pop3) 12345
  , bench "Timo's pop (fold)" $
    nftype (test pop3) 12345
  ]
