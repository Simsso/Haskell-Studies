{-# OPTIONS_GHC -Wall #-}

module Exercises where

-- String processing
-- (1)
startsWith :: (Eq a) => [a] -> [a] -> Bool
startsWith [] _ = True
startsWith _ [] = False
startsWith (s:ss) (x:xs) = if s == x then startsWith ss xs else False

startsWithThe :: String -> Bool
startsWithThe = startsWith "the"


replaceThe :: String -> String
replaceThe "" = ""
replaceThe s@(h:t) = if startsWithThe s then "a" ++ replaceThe (drop 3 s) else h : replaceThe t


-- (2)
splitAtItem :: (Eq a) => a -> [a] -> [[a]]
splitAtItem _ [] = []
splitAtItem a x@(h:t) = if h == a then splitAtItem a t else [takeWhile neqx x] ++ splitAtItem a (dropWhile neqx x) where
  neqx = (\b -> b /= a)

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel s' = go (splitAtItem ' ' s') where
  go [] = 0
  go (_:[]) = 0
  go (_:"":_) = 0
  go (s1:s2:ss) = if s1 == "the" && elem (head s2) "aeiou" then 1 + go ss else go (s2 : ss)
  
-- (3)
countVowels :: String -> Integer
countVowels "" = 0
countVowels (s:ss) = if isVowel s then 1 + countVowels ss else countVowels ss where
  isVowel = (flip elem) "aeiou"
  

-- Validate the word
newtype Word' = Word' String deriving (Eq, Show)
mkWord :: String -> Maybe Word'
mkWord s = if vowelsCount > nonVowelsCount then Nothing else Just (Word' s) where
  vowelsCount = countVowels s
  nonVowelsCount = toInteger (length s) - vowelsCount
  

-- It’s only Natural
data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ x) = 1 + natToInteger x

integerToNat :: Integer -> Maybe Nat
integerToNat i
  | i < 0 = Nothing
  | otherwise = Just (posIntToNat i) where 
    posIntToNat i'
      | i' == 0 = Zero
      | otherwise = Succ $ posIntToNat $ i' - 1
  

-- Small library for Maybe
-- (1)
isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _ = True

isNothing :: Maybe a -> Bool
isNothing = not . isJust


-- (2)
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b _ Nothing = b
mayybee _ m (Just a) = m a

-- (3)
fromMaybe :: a -> Maybe a -> a
fromMaybe b = mayybee b id

-- (4)
listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe xs = Just $ head xs

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

-- (5)
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing:xs) = catMaybes xs
catMaybes ((Just x):xs) = x : catMaybes xs

-- (6)
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Just []
flipMaybe xs = if onlyJust then Just (unpack xs) else Nothing
  where 
    onlyJust = length (takeWhile isJust xs) == length xs
    unpack [] = []
    unpack ((Just x):xs') = x : unpack xs'
    unpack _ = []
    
-- Dominik's solution (more elegant)
flipMaybe2 :: [Maybe a] -> Maybe [a]
flipMaybe2 []            = Just []
flipMaybe2 (m:ms) =
  case m of
  Nothing              -> Nothing
  Just x
    -> case flipMaybe2 ms of
       Nothing         -> Nothing
       Just xs         -> Just (x:xs)
  

-- Small library for Either
lefts' :: [Either a b] -> [a]
lefts' [] = []
lefts' xs = foldr leftList [] xs where
  leftList (Left a) xs' = a : xs'
  leftList (Right _) xs' = xs'

rights' :: [Either a b] -> [b]
rights' [] = []
rights' xs = foldl rightList [] xs where
  rightList xs' (Left _) = xs'
  rightList xs' (Right a) = a : xs'

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = (lefts' xs, rights' xs)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' m (Right b) = Just $ m b

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' ac _ (Left a) = ac a
either' _ bc (Right b) = bc b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' bc x = either (\_ -> Nothing) (\x' -> Just (bc x')) x


-- Unfolds
myIterate :: (a -> a) -> a -> [a]
myIterate m a = [a] ++ (myIterate m $ m a)


myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr m b = go (m b) where
  go Nothing = []
  go (Just (a, b')) = a : myUnfoldr m b'
  
betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\b -> Just (b, f b)) x


-- Finally something other than a list!
data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold m a = go $ m a where
  go Nothing = Leaf
  go (Just (x1, b, x2)) = Node (unfold m x1) b (unfold m x2)
  

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = go 0 where
  go i
    | i >= n = Leaf
    | otherwise  = Node subTree i subTree where
      subTree = go (i+1)
      
-- Dominik's solution using unfold
treeBuild2 :: Integer -> BinaryTree Integer
treeBuild2 n = unfold f n
  where f :: Integer -> Maybe (Integer, Integer, Integer)
        f x = if x > 0 then Just (x-1, n-x, x-1) else Nothing
        
