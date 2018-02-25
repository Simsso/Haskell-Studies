{-# OPTIONS_GHC -Wall #-}

import Control.Monad (join)


-- fmap :: Functor f => (a -> b) -> f a -> f b
-- join :: Monad m => m (m a) -> m a
bind :: Monad m => (a -> m b) -> m a -> m b
bind f a = join $ fmap f a

twiceWhenEven :: Integral a => [a] -> [a]
twiceWhenEven xs = do
  x <- xs
  if even x
    then [x*x, x*x]
    else return $ x*x