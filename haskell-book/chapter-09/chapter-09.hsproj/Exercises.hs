module Exercises where

import Data.Bool
import Data.Char

-- EnumFromTo (PDF page 339)

eftBool :: Bool -> Bool -> [Bool]
eftBool = eft

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd = eft

eftInt :: Int -> Int -> [Int]
eftInt = eft

eftChar :: Char -> Char -> [Char]
eftChar = eft

eft :: (Enum a) => a -> a -> [a]
eft a b
  | ai < bi  = a : eft (toEnum . succ $ ai) b
  | ai == bi = [a]
  | otherwise = []
  where 
    ai = fromEnum a
    bi = fromEnum b
    


-- Thy Fearful Symmetry (PDF page 343)
-- (1)
splitAtChar :: Char -> String -> [String]
splitAtChar c "" = []
splitAtChar c s  = takeWhile (\a -> a /= c) s : 
  (splitAtChar c (drop 1 (dropWhile (\a -> a /= c) s)))
splitSpace :: String -> [String]
splitSpace = splitAtChar ' '

-- (2) 
myLines = splitAtChar '\n'


-- Square Cube (PDF page 349)
mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]
-- (1)
mySqrCubeTuples = [(x, y) | x <- mySqr, y <- myCube]
-- (2)
mySqrCubeTuples2 = [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]
-- (3)
count = length mySqrCubeTuples2


-- Bottom Madness (PDF page 358)
-- not very certain
-- [1, 2, 3, 4, 5]  -- normal form
-- 1 : 2 : 3 : 4 : _  -- weak head normal form
-- enumFromTo 1 10  -- neither
-- length [1, 2, 3, 4, 5]  -- neither
-- sum (enumFromTo 1 10)  -- neither
-- ['a'..'m'] ++ ['n'..'z']  -- weak head normal form
-- (_, 'b')  -- normal form


-- More Bottoms (PDF page 365)
-- String to [Bool] where vocals are True
itIsMystery xs = map (\x -> elem x "aeiou") xs

myBool :: a -> a -> Bool -> a
myBool x y b = undefined  -- what is the task?



-- Filtering (PDF page 268)
multipleOf3 = filter (\x -> rem x 3 == 0) [1..30]
myFilter xs = filter (\x -> not (elem x ["a", "the", "an"])) (splitSpace xs)


-- Zipping Exercisese (PDF page 370)
myZip :: [a] -> [b] -> [(a, b)]
myZip _ []          = []
myZip [] _          = []
myZip (a:as) (b:bs) = (a, b) : myZip as bs

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ _ [] = []
myZipWith _ [] _ = []
myZipWith f (a:as) (b:bs) = f a b : myZipWith f as bs

myZip2 = myZipWith (\a -> \b -> (a, b))




-- Chapter Exercises (PDF page 371)
-- Data.Char
-- (2)
filterUpper s = filter isUpper s
-- (3)
capitalize []     = []
capitalize (s:ss) = toUpper s : ss
-- (4)
stringToUpper "" = ""
stringToUpper (s:ss) = toUpper s : stringToUpper ss
-- (5)
getFirstCapitalized "" = ' ' :: Char
getFirstCapitalized (s:_) = toUpper s
-- (6) 
getFirstCapitalized2 s = (toUpper . head) s
getFirstCapitalized3 = toUpper . head


-- Writing your own standard functions (PDF page 374)
-- (1)
myOr :: [Bool] -> Bool
myOr []     = False
myOr (x:xs) = x || myOr xs
-- (2)
myAny :: (a -> Bool) -> [a] -> Bool
myAny _ []     = False
myAny f (x:xs) = f x || myAny f xs
-- (3)
myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem a (x:xs) = x == a || myElem a xs
-- (4)
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]
-- (5)
squish :: [[a]] -> [a]
squish [] = []
squish (xs:xss) = xs ++ squish xss
-- (6)
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f xs = squish (map f xs)
-- (7)
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id
-- (8) weird exercise ?!
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy o []         = undefined
myMaximumBy o (x1:[])    = x1
-- myMaximumBy o (x1:x2:[]) = max x1 x2
-- myMaximumBy o (x1:x2:xs) = max (max x1 x2) myMaximumBy xs

-- (9)
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a;
myMinimumBy = undefined
-- (10)