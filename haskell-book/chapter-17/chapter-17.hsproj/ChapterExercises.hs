{-# OPTIONS_GHC -Wall #-}

module ChapterExercises where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
  
-- Check back with Dominik
-- Type
-- []
-- Methods
-- pure :: a -> [a]
-- (<*>) :: ? (a -> b) -> ? a -> ? b

-- Type
-- IO
-- Methods
-- pure :: a -> IO a
-- (<*>) :: IO (a -> b) -> IO a -> IO b

-- Type
-- (,) b
-- Methods
-- pure :: a -> (c,a)
-- (<*>) :: (c,(a -> b)) -> (c,a) -> (c,b)

-- Type
-- (->) e
-- Methods
-- pure :: a -> (e -> a)
-- (<*>) :: (e -> (a -> b)) -> (e -> a) -> (e -> b)


-- Instances
-- (1)
data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)
  
instance Applicative Pair where
  pure x = Pair x x
  (<*>) (Pair f g) (Pair a b) = Pair (f a) (g b)

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Pair a b
    
instance (Eq a) => EqProp (Pair a) where
  (=-=) = eq
  
-- (2)
data Two a b = Two a b

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)
  
instance (Monoid a) => Applicative (Two a) where
  pure x = Two mempty x
  (<*>) (Two a f) (Two b x) = Two (mappend a b) (f x)
  



-- (3)
data Three a b c = Three a b c

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)
  
instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure x = Three mempty mempty x
  (<*>) (Three a b c) (Three d e f) = 
    Three (mappend a d) (mappend b e) (c f)


-- (4)
data Three' a b = Three' a b b

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)
  
instance (Monoid a) => Applicative (Three' a) where
  pure x = Three' mempty x x
  (<*>) (Three' a b c) (Three' d e f) = 
    Three' (mappend a d) (b e) (c f)


-- (5)
data Four a b c d = Four a b c d
  
instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)
  
instance (Monoid a, Monoid b, Monoid c) => 
  Applicative (Four a b c) where
  pure x = Four mempty mempty mempty x
  (<*>) (Four a b c d) (Four e f g h) =
    Four (mappend a e) (mappend b f) (mappend c g) (d h)


-- (6)
data Four' a b = Four' a a a b

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)
  
instance (Monoid a) => Applicative (Four' a) where
  pure x = Four' mempty mempty mempty x
  (<*>) (Four' a b c d) (Four' e f g h) =
    Four' (mappend a e) (mappend b f) (mappend c g) (d h)
  


-- Tests
pair :: Pair (Int, Int, Int)
pair = undefined

main :: IO ()
main = do
  quickBatch $ applicative pair
