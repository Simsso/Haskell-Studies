{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Data.Monoid (mconcat)
import Control.Applicative

main = scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word"
    html (mconcat
      [ "<h1>URL param ", beam, "</h1>"])
      

data F a b = F (a -> b)

instance Functor (F a) where
  fmap f (F g) = F (f . g)

instance Applicative (F a) where
  pure x = F (const x)
  (F f) <*> (F g) = F (\x -> f x (g x))

-- f :: a -> (b -> c)

-- (<*>) :: (a -> (b -> c)) -> (a -> b) -> (a -> c)
--             f                  g        x -> ?