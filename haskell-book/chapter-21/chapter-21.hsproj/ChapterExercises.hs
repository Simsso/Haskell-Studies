{-# OPTIONS_GHC -Wall #-}

module ChapterExercises where
  
import Test.QuickCheck
import Test.QuickCheck.Checkers

import Data.Monoid


-- Identity

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)
  
instance Functor Identity where
  fmap f (Identity x) = Identity $ f x
  
instance Foldable Identity where
  foldr f b (Identity x) = f x b

instance Traversable Identity where
  traverse f (Identity x) = fmap Identity $ f x
  
instance (Arbitrary x) => Arbitrary (Identity x) where
  arbitrary = Identity <$> arbitrary
    
instance (Eq x) => EqProp (Identity x) where
  (=-=) = eq
  

-- Constant

newtype Constant a b = Constant { getConstant :: a }
  deriving (Eq, Show)
  
instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x
  
instance Foldable (Constant a) where
  foldr _ b _ = b

instance Traversable (Constant a) where
  traverse _ (Constant x) = pure (Constant x)
  --traverse _ x = pure x

instance (Arbitrary a) => Arbitrary (Constant a b) where
  arbitrary = fmap Constant arbitrary
    
instance (Eq a) => EqProp (Constant a b) where
  (=-=) = eq


-- Maybe

data Optional a = Nada | Yep a deriving (Eq, Show)

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep a) = Yep $ f a

instance Foldable Optional where
  foldr _ b Nada = b
  foldr f b (Yep a) = f a b
  
instance Traversable Optional where
  traverse _ Nada = pure Nada
  traverse f (Yep a) = fmap Yep $ f a
  
instance (Arbitrary x) => Arbitrary (Optional x) where
  arbitrary = do
    a <- arbitrary
    frequency [(1, return Nada), (1, return (Yep a))]

instance (Eq x) => EqProp (Optional x) where
  (=-=) = eq


-- List

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)
  
instance Foldable List where
  foldr _ b Nil = b
  foldr f b (Cons a l) = foldr f (f a b) l
  foldMap _ Nil = mempty
  foldMap f (Cons a l) = mappend (f a) $ foldMap f l

instance Traversable List where
  traverse _ Nil = pure Nil
  traverse f (Cons a l) = fmap Cons (f a) <*> traverse f l
  --traverse f (Cons a l) = fmap Cons (f a) *> traverse f l
  
append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

instance Applicative List where
  pure a      = Cons a Nil
  (<*>) (Cons f fs) xa@(Cons x xs) = Cons (f x) $ 
    append (fmap f xs) (fs <*> xa)
  (<*>) _ _ = Nil
    
take' :: Int -> List a -> List a
take' 0 _ = Nil
take' _ Nil = Nil
take' n (Cons x xs) = Cons x $ take' (n-1) xs

instance (Eq a) => EqProp (List a) where
  xs =-= ys = eq xFinite yFinite
    where xFinite = take' 100 xs
          yFinite = take' 100 ys

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = do
    x <- arbitrary
    l <- arbitrary
    frequency [(1, return Nil),
      (5, return $ Cons x l)]
      

-- Three

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b $ f c

instance Foldable (Three a b) where
  foldMap f (Three _ _ a) = f a
  foldr f b (Three _ _ a) = f a b
  
instance Traversable (Three a b) where
  traverse f (Three a b c) = fmap (Three a b) $ f c
  
instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b, Arbitrary c) => 
  Arbitrary (Three a b c) where
    arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary
      

-- Pair (skipped)


-- Big

data Big a b = Big a b b deriving (Eq, Show)

instance Functor (Big a) where
  fmap f (Big a b c) = Big a (f b) (f c)
  
instance Foldable (Big a) where
  foldr f b (Big _ a1 a2) = f a2 $ f a1 b
  foldMap f (Big _ a1 a2) = (f a1) <> (f a2)
  
instance Traversable (Big a) where
  traverse f (Big a b1 b2) = fmap (Big a) (f b1) <*> f b2
  
instance (Eq a, Eq b) => EqProp (Big a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = Big <$> arbitrary <*> arbitrary <*> arbitrary


-- Bigger

data Bigger a b = Bigger a b b b deriving (Eq, Show)

instance Functor (Bigger a) where
  fmap f (Bigger a b c d) = Bigger a (f b) (f c) (f d)
  
instance Foldable (Bigger a) where
  foldr f b (Bigger _ a1 a2 a3) = f a3 $ f a2 $ f a1 b
  foldMap f (Bigger _ a b c) = f a <> f b <> f c
  
instance Traversable (Bigger a) where
  traverse f (Bigger a b1 b2 b3) = fmap (Bigger a) (f b1) <*> f b2 <*> f b3
  
instance (Eq a, Eq b) => EqProp (Bigger a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Bigger a b c d
