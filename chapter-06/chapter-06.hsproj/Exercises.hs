module Exercises where
  
import Data.List

  
-- Eq Instances (PDF page 210)
-- ===========================

-- (1)
data TisAnInteger = TisAn Integer
instance Eq TisAnInteger where
  (==) (TisAn i) (TisAn i') = i == i'
  
-- (2)
data TwoIntegers = Two Integer Integer
instance Eq TwoIntegers where
  (==) (Two i1 i2) (Two i1' i2') = i1 == i1' && i2 == i2'
  
-- (3)
data StringOrInt = TisAnInt Int | TisAString String
instance Eq StringOrInt where
  (==) (TisAnInt i) (TisAnInt i') = i == i'
  (==) (TisAString s) (TisAString s') = s == s'
  (==) _ _ = False
  
-- (4)
data Pair a = Pair a a
instance Eq a => Eq (Pair a) where
  (==) (Pair a b) (Pair a' b') = a == a' && b == b'
  
-- (5)
data Tuple a b = Tuple a b
instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple a b) (Tuple a' b') = a == a' && b == b'
  
-- (6)
data Which a = ThisOne a | ThatOne a
instance (Eq a) => Eq (Which a) where
  (==) (ThisOne a) (ThisOne a') = a == a'
  (==) (ThatOne a) (ThatOne a') = a == a'
  (==) (ThisOne a) (ThatOne a') = a == a'
  (==) (ThatOne a) (ThisOne a') = a == a'
  
-- (7)
data EitherOr a b = Hello a | Goodbye b
instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello a) (Hello a') = a == a'
  (==) (Goodbye a) (Goodbye a') = a == a'
  (==) _ _ = False
  

-- Chapter Exercises (PDF page 238)
-- ================================
-- (1) c
-- (2) a b
-- (3) a
-- (4) c
-- (5) a


-- Does it typecheck? (PDF page 239)
-- =================================

x :: Int -> Int
x blah = blah + 20

printIt :: Int -> IO ()
printIt x = putStrLn (show x)

-- (1)
data Person = Person Bool
  deriving (Show)
  
printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

-- (2)
data Mood = Blah | Woot deriving (Show, Eq)
settleDown x = if x == Woot then Blah else x

-- (3)
-- a) Blah or Woot
-- b) It will throw an error because Num can not be compared to Mood
-- c) Wont work because Mood does not derive Ord

-- (4)
type Subject = String
type Verb = String
type Object = String
data Sentence = Sentence Subject Verb Object
  deriving (Eq, Show)
s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"


-- Given a datatype declaration, what can we do? (PDF page 240)
-- ============================================================
data Rocks = Rocks String   deriving (Eq, Show)
data Yeah = Yeah Bool       deriving (Eq, Show)
data Papu = Papu Rocks Yeah deriving (Eq, Show)

-- (1)
-- phew = Papu "chases" True
phew = Papu (Rocks "chases") (Yeah True)

-- (2)
truth = Papu (Rocks "chomskydoz") (Yeah True)

-- (3)
equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'

-- (4)
comparePapus :: Papu -> Papu -> Bool; comparePapus = undefined
-- comparePapus p p' = p > p'
-- Papu does not implement Ord


-- Match the types
-- ===============
-- (1)
i :: Num a => a
-- i :: a
i = 1

-- (2)
f1 :: Float
--f :: Num a => a
f1 = 1.0

-- (3)
-- f2 :: Float
f2 :: Fractional a => a
f2 = 1.0

-- (4)
-- f3 :: Float
f3 :: RealFrac a => a
f3 = 1.0

-- (5)
-- freud :: a -> a
freud :: Ord a => a -> a
freud x = x

-- (6)
-- freud' :: a -> a
freud' :: Int -> Int
freud' x = x

-- (7)
myX = 1 :: Int
sigmund :: Int -> Int
-- sigmund :: a -> a
sigmund x = myX

-- (8) not working with the new signature -- why?
sigmund' :: Int -> Int
-- sigmund' :: Num a => a -> a
sigmund' x = myX

-- (9) works because Int implements Ord
-- jung :: Ord a => [a] -> a
jung :: [Int] -> Int
jung xs = head (sort xs)

-- (10)
-- young :: [Char] -> Char
young :: Ord a => [a] -> a
young xs = head (sort xs)

-- (11) does not work since mySort expects Char
mySort :: [Char] -> [Char]
mySort = sort
signifier :: [Char] -> Char
-- signifier :: Ord a => [a] -> a
signifier xs = head (mySort xs)


-- Type-Kwon-Do Two: Electric Typealoo
-- ===================================
-- (1)
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk atob a b = atob a == b

-- (2) how to handle the Integer?
arith :: Num b => (a -> b) -> Integer -> a -> b
arith atob i a = atob a
