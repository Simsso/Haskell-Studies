{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts #-}

module SkiFree where
  
import Test.QuickCheck
import Test.QuickCheck.Checkers

-- contains one value of a
-- and another data structure which contains "as" as well.
data S n a = S (n a) a deriving (Eq, Show)

instance (Functor n, Arbitrary (n a), Arbitrary a )
  => Arbitrary (S n a) where
    arbitrary =
      S <$> arbitrary <*> arbitrary

instance (Eq (n a), Eq a) => EqProp (S n a) where
  (=-=) = eq
    
instance (Functor n) => Functor (S n) where
  fmap f (S n a) = S (fmap f n) (f a)
  
instance (Foldable n) => Foldable (S n) where
  foldr f b (S n a) = foldr f (f a b) n
  foldMap f (S n a) = mappend (foldMap f n) (f a)

instance Traversable n => Traversable (S n) where
  traverse f (S n a) = (fmap S (traverse f n)) <*> (f a)

main :: IO [S [] Int]
main = sample' (arbitrary :: Gen (S [] Int))