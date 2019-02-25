{-# OPTIONS_GHC -Wall #-}

-- Semigroup and Monoid exercises
module Exercises where
  
import Test.QuickCheck (
  arbitrary, frequency,
  Arbitrary, CoArbitrary)
import Data.Semigroup
import qualified Data.Monoid as M


-- validation
assoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
assoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

leftId :: (Eq m, M.Monoid m) => m -> Bool
leftId a = (mempty M.<> a) == a

rightId :: (Eq m, M.Monoid m) => m -> Bool
rightId a = (a M.<> mempty) == a

---------------
-- Semigroup
---------------

-- (1)
data Trivial = Trivial deriving (Eq, Show)
instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial
  
-- (2)
newtype Identity a = Identity a deriving (Eq, Show)
instance Semigroup (Identity a) where
  (Identity a) <> _ = Identity a
  
instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a
    
-- (3)
-- testing for Integer either with Sum or Product
data Two a b = Two a b
  deriving (Eq, Show)
instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two a' b') = Two (a <> a') (b <> b')
  
instance (Arbitrary a, Arbitrary b) =>
  Arbitrary (Two a b) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      return $ Two a b

-- (4)
data Three a b c = Three a b c
instance (Semigroup a, Semigroup b, Semigroup c) =>
  Semigroup (Three a b c) where
    (Three a1 b1 c1) <> (Three a2 b2 c2) =
      Three (a1 <> a2) (b1 <> b2) (c1 <> c2)
      
-- (5) continue pattern from (3) and (4)
      
-- (6)
newtype BoolConj = BoolConj Bool deriving (Eq, Show)
instance Semigroup BoolConj where
  (BoolConj False) <> _ = BoolConj False
  _ <> (BoolConj False) = BoolConj False
  _ <> _                = BoolConj True

instance Arbitrary BoolConj where
  arbitrary = frequency
    [ (1, return $ BoolConj True),
      (1, return $ BoolConj False) ]

-- (7)
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)
instance Semigroup BoolDisj where
  (BoolDisj True) <> _ = BoolDisj True
  _ <> (BoolDisj True) = BoolDisj True
  _ <> _               = BoolDisj True

instance Arbitrary BoolDisj where
  arbitrary = frequency
    [ (1, return $ BoolDisj True),
      (1, return $ BoolDisj False) ]

-- (8)
data Or a b = Fst a | Snd b deriving (Eq, Show)
instance Semigroup (Or a b) where
  (Snd a) <> _ = Snd a
  _ <> (Snd a) = Snd a
  _ <> (Fst a) = Fst a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [
      (1, return (Fst a)),
      (1, return (Snd b)) ]


-- (9)
newtype Combine a b = Combine { unCombine :: (a -> b) }

instance (Semigroup b) => Semigroup (Combine a b) where
  (Combine { unCombine = unCombine1 }) <> 
    (Combine { unCombine = unCombine2 }) = 
      Combine { unCombine = unCombine1 <> unCombine2 }

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = do
    a <- arbitrary
    return $ Combine a
    

-- (10)
newtype Comp a = Comp { unComp :: (a -> a) }

instance (Semigroup a) => Semigroup (Comp a) where
  Comp a <> Comp b = Comp $ a <> b


-- (11)
data Validation a b = Failure a | Success b 
  deriving (Eq, Show)
  
instance Semigroup a => Semigroup (Validation a b) where 
  (<>) (Success a) _ = Success a
  (<>) _ (Success a) = Success a
  (<>) (Failure a1) (Failure a2) = Failure (a1 <> a2)

main :: IO ()
main = do
  let failure :: String -> Validation String Int
      failure = Failure
      success :: Int -> Validation String Int
      success = Success
  print $ success 1 <> failure "blah"
  print $ failure "woot" <> failure "blah"
  print $ success 1 <> success 2
  print $ failure "woot" <> success 2




-------------
-- Monoid
-------------

-- (1)
instance Monoid Trivial where
  mempty      = Trivial
  mappend _ _ = Trivial

-- (2)
-- does not appear to be a Monoid because no mempty exists
instance (Monoid a) => Monoid (Identity a) where
  mempty = undefined
  mappend (Identity a) (Identity b) = Identity $ a M.<> b
  
-- (3)
-- does not appear to have an identity either
instance (Monoid a, Monoid b) => 
  Monoid (Two a b) where
    mempty = undefined
    mappend = undefined
    

-- (4) corresponds to (7) from above
instance Monoid BoolConj where
  mempty = BoolConj True
  mappend (BoolConj a) (BoolConj b) = BoolConj $ a && b


-- (5)
instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend (BoolDisj a) (BoolDisj b) = BoolDisj $ a || b

-- (6)
instance (Monoid a, Monoid b) => Monoid (Combine a b) where
  mempty  = Combine mempty
  mappend (Combine a) (Combine b) =
    Combine (mappend a b)

-- (7)
instance (Monoid a) => Monoid (Comp a) where
  mempty = Comp mempty
  mappend (Comp a) (Comp b) = Comp (mappend a b)






