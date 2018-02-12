-- Finally figured what the offset between
-- book and PDF pages is: 2^5

{-# LANGUAGE FlexibleInstances #-}


module Exercises where
  
import qualified FunctorTest as FT
import Test.QuickCheck
import Test.QuickCheck.Function
  
-- Be Kind (PDF page 666)
-- (1) a :: *
-- (2) b :: * -> *
--     T :: * -> *  (check back)
-- (3) c :: * -> * -> *


-- Heavy Lifting (PDF page 688)
-- (5)
e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = fmap (read . ("123"++) . show) ioi
    in fmap (*3) changed
    

-- Instances of Func (PDF page 695)
-- (1)
newtype Identity a = Identity a 
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)
  
instance (Arbitrary a) => 
  Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a
    
-- (2)
data Pair a = Pair a a 
  deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

instance (Arbitrary a) => 
  Arbitrary (Pair a) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      return $ Pair a b
      
-- (3)
data Two a b = Two a b
  deriving (Eq, Show)
  
instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)
  
-- Is Arbitrary (Two a b) necessary
-- even though a is not needed at all?
instance (Arbitrary a, Arbitrary b) =>
  Arbitrary (Two a b) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      return $ Two a b

-- (4)
data Three a b c = Three a b c
  deriving (Eq, Show)
  
instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)
  
instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
  Arbitrary (Three a b c) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      c <- arbitrary
      return $ Three a b c 

-- (5)
data Three' a b = Three' a b b 
  deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)
  
instance (Arbitrary a, Arbitrary b) =>
  Arbitrary (Three' a b) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      c <- arbitrary
      return $ Three' a b c 
      
-- (6)
data Four a b c d = Four a b c d
  deriving (Eq, Show)
  
instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)
  
instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
  Arbitrary (Four a b c d) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      c <- arbitrary
      d <- arbitrary
      return $ Four a b c d

-- (7)
data Four' a b = Four' a a a b
  deriving (Eq, Show)
  
instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)
  
instance (Arbitrary a, Arbitrary b) =>
  Arbitrary (Four' a b) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      c <- arbitrary
      d <- arbitrary
      return $ Four' a b c d
      
-- (8)
-- cannot be implemeted because Trivial
-- has the kind *, where * -> * is required
data Trivial = Trivial
  deriving (Eq, Show)


-- Possibly (PDF page 698)
data Possibly a = Nope | Yeppers a
  deriving (Eq, Show)

instance Functor Possibly where
  fmap _ Nope = Nope
  fmap f (Yeppers a) = Yeppers (f a)
  

-- Short Exercise (PDF page 700)
data Sum a b = First a | Second b 
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)
  
-- (2)
-- because the kind * -> * -> * needs to be reduced to * -> * and that requires the specification of the type of First



--------------------------------------
-- Chapter Exercises (PDF page 711)
--------------------------------------

-- Valid?
-- (1) NO
-- (2) YES
-- (3) YES
-- (4) YES (?)
-- (5) NO

-- Rearrange
-- (1)
data Sum2 a b = First2 a | Second2 b
instance Functor (Sum2 e) where
  fmap _ (First2 a) = First2 a
  fmap f (Second2 b) = Second2 (f b)
  
-- (2)
data Company a b c = DeepBlue a c | Something b

instance Functor (Company e e') where
  fmap _ (Something b) = Something b
  fmap f (DeepBlue a c) = DeepBlue a (f c)
  
-- (3)
data More a b = L a b a | R b a b
  deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L a (f b) a'
  fmap f (R b a b') = R (f b) a (f b')
  

-- Write functor instances
-- (1)
data Quant a b = Finance | Desk a | Bloor b
instance Functor (Quant a) where
  fmap _ Finance   = Finance
  fmap _ (Desk a)  = Desk a
  fmap f (Bloor b) = Bloor (f b)

-- (2)
data K a b = K a

instance Functor (K a) where
  fmap _ (K a) = K a
  
-- (3)
newtype Flip f a b = Flip (f b a)
  deriving (Eq, Show)

instance Functor (Flip K a) where
  fmap f (Flip (K a)) = Flip $ K $ f a
  
-- (4)
data EvilGoateeConst a b = GoatyConst b

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)
  
-- (5)
data LiftItOut f a = LiftItOut (f a)

instance (Functor fa) => Functor (LiftItOut (fa)) where
  fmap f (LiftItOut a) = LiftItOut (fmap f a)

-- (6)
data Parappa f g a = DaWrappa (f a) (g a)
instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap h (DaWrappa a b) = DaWrappa (fmap h a) (fmap h b)

-- (7)
data IgnoreOne f g a b = IgnoringSomething (f a) (g b)
instance (Functor g) => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething a b) = IgnoringSomething a (fmap f b)

-- (8)
data Notorious g o a t = Notorious (g o) (g a) (g t)
instance (Functor g) => Functor (Notorious g o a) where
  fmap f (Notorious a b c) = Notorious a b (fmap f c)

-- (9)
data List a = Nil | Cons a (List a)
instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

-- (10)
data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat (f a)
  fmap f (MoreGoats a b c) = MoreGoats (fmap f a) (fmap f b) (fmap f c)

-- (11)
data TalkToMe a = Halt | Print String a | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print s a) = Print s (f a)
  fmap f (Read sa) = Read (f . sa)
  