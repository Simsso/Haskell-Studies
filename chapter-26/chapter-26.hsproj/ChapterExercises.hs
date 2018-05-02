{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE InstanceSigs #-}

module ChapterExercises where
  
import Control.Monad.Trans.Reader
import Data.Functor.Identity
import StateT
  
rDec :: Num a => Reader a a
rDec = ReaderT $ \r -> pure $ r - 1

dec :: Num a => a -> a
dec = flip (-) 1

rDec' :: Num a => Reader a a
rDec' = ReaderT $ pure . dec

rShow :: Show a => ReaderT a Identity String
rShow = ReaderT $ \a -> Identity $ show a

rShow' :: Show a => ReaderT a Identity String
rShow' = ReaderT $ Identity . show

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = ReaderT $ \a -> putStrLn ("Hi " ++ show a) >> pure (a + 1)

sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = StateT $ \s -> putStrLn (show s) >> pure (show s, s + 1)

