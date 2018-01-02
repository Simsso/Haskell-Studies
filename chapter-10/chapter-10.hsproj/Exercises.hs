module Exercises where

-- Understanding Folds (PDF page 398)
-- (3) c) foldr, but not foldl, associates to the right
-- (4) a) folds reduce structures


-- Database Processing (PDF page 403)
import Data.Time
data DatabaseItem = DbString String 
  | DbNumber Integer
  | DbDate UTCTime
  deriving (Eq, Ord, Show)
theDatabase :: [DatabaseItem]
theDatabase =
  [ 
    DbDate (UTCTime
      (fromGregorian 1911 5 1)
      (secondsToDiffTime 34123))
    , DbNumber 9001
    , DbString "Hello, world!"
    , DbDate (UTCTime 
      (fromGregorian 1921 5 1)
      (secondsToDiffTime 34123))
  ]


foldFilterDate :: DatabaseItem -> [UTCTime] -> [UTCTime]
foldFilterDate (DbDate time) bs = time : bs
foldFilterDate _ bs = bs

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr foldFilterDate []


foldFilterNumber :: DatabaseItem -> [Integer] -> [Integer]
foldFilterNumber (DbNumber n) bs = n : bs
foldFilterNumber _ bs = bs

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr foldFilterNumber []


foldMostRecent :: DatabaseItem -> UTCTime -> UTCTime
foldMostRecent (DbDate utc) oldMax = max utc oldMax
foldMostRecent _ oldMax = oldMax

-- which base case to choose?
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = foldr foldMostRecent (UTCTime 
      (fromGregorian 0 0 0)
      (secondsToDiffTime 0))
      
sumPm :: DatabaseItem -> Integer -> Integer
sumPm (DbNumber n) x = n + x
sumPm _ x = x
sumDb :: [DatabaseItem] -> Integer
sumDb = foldr sumPm 0

countNumbers :: [DatabaseItem] -> Int
countNumbers [] = 0
countNumbers ((DbNumber x):xs) = 1 + countNumbers xs
countNumbers (x:xs) = countNumbers xs
avgDb :: [DatabaseItem] -> Double
avgDb db = (fromIntegral (sumDb db)) / (fromIntegral (countNumbers db))


-- Scans Exercises (PDF page 411)
fibs = 1 : scanl (+) 1 fibs
fibs20 = take 20 fibs
fibsLt100 = takeWhile (\x -> x < 100) fibs
-- factorial with scanl?


-- Chapter Exercises (PDF page 411)
-- (1)
stops = "pbtdkg"
vowels = "aeiou"
combinations a b = [x ++ " " ++ y ++ " " ++ z | x <- xs, y <- xs, z <- xs] where xs = [a, b]
comb = [([s, v, s2] :: String) | s <- stops, v <- vowels, s2 <- stops]
comb2 = [([s, v, s2] :: String) | s <- stops, v <- vowels, s2 <- stops]

nouns = ["car", "plane", "workplace"]
verbs = ["walk", "run", "come"]
nvTuples = [(n, v) | n <- nouns, v <- verbs]

-- (2) average word length (3)
seekritFunc x = (fromIntegral (sum (map length (words x)))) / (fromIntegral (length (words x)))

-- Rewriting functions using folds
myAnd :: [Bool] -> Bool
myAnd = foldr (\a -> \b -> a && b) True

myOr :: [Bool] -> Bool
myOr = foldr (\a -> \b -> a || b) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\a -> \b -> f a || b) False

myElem :: Eq a => a -> [a] -> Bool
myElem x = foldr (\a -> \b -> a == x || b) False
myElem2 x = myAny (\a -> a == x)

myReverse :: [a] -> [a]
myReverse = foldl (\b -> \x -> x : b) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x -> \xs -> f x : xs) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x -> \xs -> if f x then x : xs else xs) []

squish :: [[a]] -> [a]
squish = foldr (\x -> \xs -> x ++ xs) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\x -> \xs -> f x ++ xs) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = foldr maxFold x xs
  where maxFold a b = if f b a == GT then b else a

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:xs) = foldr minFold x xs
  where minFold a b = if f a b == LT then a else b


