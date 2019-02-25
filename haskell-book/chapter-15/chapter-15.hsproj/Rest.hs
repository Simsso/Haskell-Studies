module Rest where

import Test.QuickCheck

class Semigroup a where
  (<>) :: a -> a -> a

instance Semigroup Int where
  i <> j = i + j

instance Semigroup [a] where
  l <> r = l ++ r



newtype Combine a b = Combine { unCombine :: (a -> b) }
instance Show (Combine a b) where
  show _ = "Combine"

instance (Semigroup b) => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine $ \x -> (f x) <> (g x)

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = do
    f <- arbitrary
    return (Combine f)

type C = Combine String String
associative :: C -> C -> C -> String -> Bool
associative c1 c2 c3 s =                f s == g s
  where f = unCombine $ (c1 <> c2) <> c3
        g = unCombine $ c1 <> (c2 <> c3)



data Func = Func (Int -> Int)
instance Show Func where
  show (Func f) = "... " ++ show (f (-1)) ++ ", " ++ 
                  show (f 0) ++ ", " ++ show (f 1) ++ " ..."

instance Arbitrary Func where
  arbitrary = do
    f <- arbitrary
    return (Func f)




main :: IO ()
main = do
  quickCheck $ \c1 c2 c3 s -> associative c1 c2 c3 s
  sample (arbitrary :: Gen Func)


