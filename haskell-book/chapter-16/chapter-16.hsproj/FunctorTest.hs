module FunctorTest where
  
import Test.QuickCheck
import Test.QuickCheck.Function

-- identity fmap id = id  
identity :: (Functor f, Eq (f a)) => f a -> Bool
identity f = fmap id f == f

-- composition fmap (p . q) = (fmap p) . (fmap q)
compose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
compose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

-- composition with quick check
composeQC :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
composeQC x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)