{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE InstanceSigs #-}

module Moi where
  
newtype Moi s a = Moi { runMoi :: s -> (a, s) }

-- question: cleaner solution
instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f moi = Moi (\s -> (f (fst (runMoi moi s)), snd (runMoi moi s)))
  
instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi (\s -> (a, s))
  
  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  (<*>) (Moi f) (Moi g) = 
    Moi (\s -> ((fst (f (snd (g s))) (fst (g s)), snd (f (snd (g s))))))
  
instance Monad (Moi s) where
  return = pure
  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  (>>=) (Moi f) g = 
    Moi (\s -> (fst (runMoi (g (fst (f s))) (snd (f s))), snd (runMoi (g (fst (f s)))  (snd (f s)))))