{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE InstanceSigs #-}

module ChapterExercises where
  
import Moi

get :: Moi s s
get = Moi (\s -> (s, s))

put :: s -> Moi s ()
put s = Moi (\_ -> ((), s))

exec :: Moi s a -> s -> s
exec (Moi sa) s = snd (sa s)

eval :: Moi s a -> s -> a
eval (Moi sa) = fst . sa

modify :: (s -> s) -> Moi s ()
modify f = Moi (\s -> ((), f s))