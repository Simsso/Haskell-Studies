{-# OPTIONS_GHC -Wall #-}

module ChapterExercises where
  
import Control.Monad (join)
  
import Test.QuickCheck
import Test.QuickCheck.Checkers
  

--------------------------------
-- Monad instance and testing
--------------------------------

-- (1)
data Nope a = NopeDotJpg
  deriving (Eq, Show)

instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg
  
instance Applicative Nope where
  pure _ = NopeDotJpg
  (<*>) _ NopeDotJpg = NopeDotJpg
  
instance Monad Nope where
  return             = pure
  (>>=) NopeDotJpg _ = NopeDotJpg
  
instance EqProp (Nope a) where
  (=-=) = eq
  
instance Arbitrary (Nope a) where
  arbitrary = do
    return NopeDotJpg
    

-- (2)
-- double check, whether this one is correct
data Either2 b a = Left2 a | Right2 b
  deriving (Eq, Show)

instance Functor (Either2 b) where
  fmap _ (Right2 b) = Right2 b
  fmap f (Left2 a)  = Left2 $ f a
  
instance Applicative (Either2 b) where
  pure               = Left2
  (<*>) _ (Right2 b) = Right2 b
  (<*>) (Right2 b) _ = Right2 b
  (<*>) (Left2 f) (Left2 a)  = Left2 $ f a
  
instance Monad (Either2 b) where
  return             = pure
  (>>=) (Left2 a) f  = f a
  (>>=) (Right2 b) _ = Right2 b
  
instance (Eq b, Eq a) => EqProp (Either2 b a) where
  (=-=) = eq
  
instance (Arbitrary a, Arbitrary b) => Arbitrary (Either2 b a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1, return $ Left2 a),
      (1, return $ Right2 b)]
   

-- (3)
newtype Id a = Id a
  deriving (Eq, Ord, Show)

instance Functor Id where
  fmap f (Id a) = Id $ f a

instance Applicative Id where
  pure = Id
  (<*>) (Id f) a = fmap f a

instance Monad Id where
  return = pure
  (>>=) (Id a) f = f a
  
instance (Eq a) => EqProp (Id a) where
  (=-=) = eq
  
instance (Arbitrary a) => Arbitrary (Id a) where
  arbitrary = do
    a <- arbitrary
    return $ Id a
    

-- (4) List
append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

take' :: Int -> List a -> List a
take' 0 _ = Nil
take' _ Nil = Nil
take' n (Cons x xs) = Cons x $ take' (n-1) xs

data List a = Nil | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a l) = Cons (f a) $ fmap f l
  
instance Applicative List where
  pure a      = Cons a Nil
  (<*>) (Cons f fs) xa@(Cons x xs) = Cons (f x) $ 
    append (fmap f xs) (fs <*> xa)
  (<*>) _ _ = Nil
  
instance Monad List where
  return = pure
  (>>=) Nil _ = Nil
  (>>=) (Cons x xs) f = append (f x) $ xs >>= f

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
      

-------------------------
-- Implement functions
-------------------------

-- (1)
j :: Monad m => m (m a) -> m a
j = join

-- (2)
l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

-- (3)
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f ma mb = fmap f ma <*> mb

-- (4)
a' :: Monad m => m a -> m (a -> b) -> m b
a' = flip (<*>)

-- (5)
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _     = return []
meh (x:xs) f = fmap (:) (f x) <*> meh xs f

-- (6)
-- point-free style B-)
-- sequence :: (Monad m, Traversable t) => t (m a) -> m (t a)
flipType :: (Monad m) => [m a] -> m [a]
flipType = flip meh id





