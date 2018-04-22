{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE InstanceSigs #-}

newtype Identity a = Identity { 
  runIdentity :: a }
  
newtype Compose f g a =
  Compose { getCompose :: f (g a) }
  deriving (Eq, Show)
  
instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose a) = Compose $ (fmap .fmap) f a
  
instance (Applicative f, Applicative g)
  => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure = Compose . pure . pure -- crispy clean
  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  (Compose f) <*> (Compose a) = Compose $ (<*>) <$> f <*> a
  
instance (Foldable f, Foldable g)
  => Foldable (Compose f g) where
--foldMap :: Monoid m => (a -> m) -> t a -> m
  foldMap am (Compose a) = (foldMap . foldMap) am a
  
instance (Traversable f, Traversable g) => Traversable (Compose f g) where
--traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  traverse afb (Compose x) = fmap Compose $ sequenceA $ fmap (traverse afb) x
  sequenceA (Compose x) = fmap Compose (sequenceA $ fmap sequenceA x)
-- version of traverse with traverse only