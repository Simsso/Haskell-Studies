{-# OPTIONS_GHC -Wall #-}

module MonoidExercise8 where
  
import Data.Monoid
  
newtype Mem s a = Mem {
    runMem :: s -> (a,s)
  }

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem (\s -> (mempty,s))
  mappend (Mem m1) (Mem m2) = Mem newRunMem where
    newRunMem s = (mappend (fst m1Out) (fst m2Out), snd m2Out) where
      m1Out = m1 s
      m2Out = m2 $ snd $ m1Out
  
-- Dominik's version    
--instance Monoid a => Monoid (Mem s a) where
--  mempty = Mem $ \s0 -> (mempty, s0)
--  mappend (Mem f) (Mem g) = Mem $ \s0 ->
--    let (a1, s1) = g s0
--        (a2, s2) = f s1
--    in (mappend a1 a2, s2)
  

f' :: Num a => Mem a [Char]
f' = Mem $ \s -> ("hi", s + 1)

main :: IO ()
main = do
  let rmzero = runMem mempty (0 :: Int)
      rmleft = runMem (f' <> mempty) (0 :: Int)
      rmright = runMem (mempty <> f') (0 :: Int)
  print $ rmleft
  print $ rmright
  print $ (rmzero :: (String, Int))
  print $ rmleft == runMem f' (0 :: Int)
  print $ rmright == runMem f' (0 :: Int)
  
