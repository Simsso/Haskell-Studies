{-# OPTIONS_GHC -Wall #-}

module ApplicativeTests where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

list2 :: [([Char], [Char], Int)]
list2 = [("b", "w", 1)]

list :: List (Int, Int, Int)
list = undefined

zipList :: ZipList' (String, Int, Bool)
zipList = undefined

main :: IO ()
main = do
--  quickBatch $ applicative list
  quickBatch $ applicative list2
  quickBatch $ applicative zipList
  

-- List Applicative Exercise (PDF page 765)
data List x = Nil | Cons x (List x)
  deriving (Eq, Show)
  
append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

take' :: Int -> List a -> List a
take' = undefined

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) $ fmap f xs

instance Applicative List where
  pure x = Cons x Nil
  (<*>) (Cons f fs) xa@(Cons x xs) = Cons (f x) $ 
    append (fmap f xs) (fs <*> xa)
  (<*>) _ _ = Nil



-- ZipList Applicative Exercise (PDF page 768)

newtype ZipList' a = ZipList' ([] a)
  deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  (ZipList' xs) =-= (ZipList' ys) = eq xFinite yFinite
    where xFinite = take 3000 xs
          yFinite = take 3000 ys
          
instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure x = ZipList' $ repeat x
  (<*>) (ZipList' (f:fs)) (ZipList' (x:xs)) = 
    ZipList' $ (f x) : rest where
      ZipList' rest = ZipList' fs <*> ZipList' xs
  (<*>) _ _ = ZipList' []
  
instance (Arbitrary a) => Arbitrary (ZipList' a) where
  arbitrary = do
    a <- arbitrary
    return $ ZipList' a
