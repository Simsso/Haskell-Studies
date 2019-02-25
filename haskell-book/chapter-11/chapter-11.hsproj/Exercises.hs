{-# OPTIONS_GHC -Wall #-}

module Exercises where

import Data.Int
import Data.Char
import Data.List

-- Dog Types (PDF page 428)
data Doggies a = Husky a | Mastiff a 
  deriving (Eq, Show)
-- 1) Type constructor
-- 2) Doggies :: * -> *
-- 3) Doggies String :: *
-- 4) Num a => Doggies a
-- 5) Doggies Integer
-- 6) Doggies String
-- 7) Both
-- 8) DogueDeBordeaux a
-- 9) DogueDeBordeaux String


-- Vehicles (PDF page 431)
data Price = Price Integer
  deriving (Eq, Show)
data Manufacturer = Mini | Mazda | Tata 
  deriving (Eq, Show)
data Airline = PapuAir | CatapultsR'Us | United
  deriving (Eq, Show)
data Vehicle = Car Manufacturer Price | Plane Airline Integer
  deriving (Eq, Show) -- (5)
 
-- (2)
isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False
isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False
areCars :: [Vehicle] -> [Bool]
areCars [] = []
areCars (x:xs) = isCar x : areCars xs

-- (3)
getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m
getManu _ = undefined

-- (4)
-- It will raise an exception. Is this a good solution?



-- Cardinality (PDF page 436)
-- (1) 1
-- (2) 3
-- (3) 2^16 = 65,536
-- (5) Number of bits used to store the integer.



-- For Example (PDF page 437)
data Example = MakeExample deriving Show
data Example2 a = MakeExample2 Int deriving Show


--  Logic Goats (PDF page 442)
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
class TooMany a where
  tooMany :: a -> Bool
  
instance TooMany Int where
  tooMany n = n > 42
  
-- (1)
data IntString a b = IntString Int String
instance TooMany (IntString a b) where
  tooMany (IntString a _) = tooMany a

-- (2)
data IntInt a = IntInt Int Int
instance TooMany (IntInt a) where
  tooMany (IntInt a b) = tooMany (a + b)

-- (3)
-- instance TooMany ((Num a, TooMany b) => (a, b)) where
--  tooMany (a, b) = tooMany a && tooMany a



-- Pity the Bool (PDF page 444)
-- (1)
data BigSmall = Big Bool | Small Bool
  deriving (Eq, Show)
-- cardinality 2 + 2 = 4

-- (2)
data NumberOrBool = Numba Int8 | BoolyBool Bool
  deriving (Eq, Show)
myNumba = Numba (-128)
-- cardinality 256 + 2 = 258


-- How Does Your Garden Grow? (PDF page 452)
data FlowerType = Gardenia | Daisy | Rose | Lilac
  deriving Show
type Gardener = String
data Garden = Garden Gardener FlowerType
  deriving Show

-- sum of products
-- data GardenSOP = GardenSOPA Gardener Gardenia | GardenSOPB Gardener Daisy | GardenSOPC Gardener Rose | GardenSOPD Gardener Lilac



-- Programmers (PDF page 462)
data OperatingSystem = GnuPlusLinux | OpenBSDPlusNevermindJustBSDStill | Mac | Windows
  deriving (Eq, Show)
data ProgLang = Haskell | Agda | Idris | PureScript
  deriving (Eq, Show)
data Programmer = Programmer { os :: OperatingSystem, lang :: ProgLang }
  deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems = [GnuPlusLinux, OpenBSDPlusNevermindJustBSDStill, Mac, Windows]
allLanguages :: [ProgLang]
allLanguages = [Haskell, Agda, Idris, PureScript]
allProgrammers :: [Programmer]
allProgrammers = [Programmer { os = o, lang = l } | o <- allOperatingSystems, l <- allLanguages]


-- The Quad (PDF page 472)
-- (1)
data Quad = One | Two | Three | Four  -- #4
  deriving (Eq, Show)
eQuad :: Either Quad Quad; eQuad = undefined  -- #4
prodQuad :: (Quad, Quad); prodQuad = undefined  -- #4*4=16
funcQuad :: Quad -> Quad; funcQuad = undefined  -- #4^4=256
prodTBool :: (Bool, Bool, Bool); prodTBool = undefined  -- #2*2*2=16
gTwo :: Bool -> Bool -> Bool; gTwo = undefined  -- #2^4=16
fTwo :: Bool -> Quad -> Quad; fTwo = undefined  -- #4^8=65536



-- Write map for BinaryTree (PDF page 480)
data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a = Node (insert' b left) a right
  | b > a = Node left a (insert' b right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)
mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)


-- Convert binary trees to lists (PDF page 482)
preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node l a r) = [a] ++ preorder l ++ preorder r

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node l a r) = preorder l ++ [a] ++ preorder r

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node l a r) = (preorder l) ++ (preorder r) ++ [a]

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder = if preorder testTree == [2, 1, 3] then putStrLn "Preorder fine!" else putStrLn "Bad news bears."
testInorder :: IO ()
testInorder = if inorder testTree == [1, 2, 3] then putStrLn "Inorder fine!" else putStrLn "Bad news bears."
testPostorder :: IO ()
testPostorder = if postorder testTree == [1, 3, 2] then putStrLn "Postorder fine!" else putStrLn "postorder failed check"


foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ b Leaf = b
foldTree f b (Node l a r) = f a (foldTree f (foldTree f b r) l)


-- Chapter Exercises (PDF page 484)
-- (1) a)
-- (2) c)
-- (3) b)
-- (4) c)

-- checks whether the first argument is a sublist of the second
isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf as bs@(_:bt) = isStartSubsetOf as bs || isSubseqOf as bt where
  isStartSubsetOf :: (Eq a) => [a] -> [a] -> Bool
  isStartSubsetOf [] _ = True
  isStartSubsetOf _ [] = False
  isStartSubsetOf (a':at') (b':bt') = a' == b' && isStartSubsetOf at' bt'

splitOn c s  = takeWhile (\a -> a /= c) s : (splitOn c (drop 1 (dropWhile (\a -> a /= c) s)))
splitSpace :: String -> [String]
splitSpace = splitOn ' '


capitalizeWords :: String -> [(String, String)]
capitalizeWords s = map firstUpper $ (splitOn ' ' s :: [String]) where
  firstUpper [] = ([], [])
  firstUpper (x:xs) = (toUpper x : xs, x:xs)

--- Language exercises
capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord (x:xs) = toUpper x : xs


-- not working yet
splitAtSeq :: (Eq a) => [a] -> [a] -> [[a]]
splitAtSeq [] _ = []
splitAtSeq x [] = [x]
splitAtSeq x@(xh:xs) s@(sh:ss) = if match 
  then [s] ++ (splitAtSeq xstart s) 
  else [xh] : splitAtSeq xs s
  where
    match = if length s > length x then False else s == xstart
    xstart = take (length s) x

capSentences :: String -> String
capSentences "" = ""

data Button = Button Char [Char]
buttons :: [Button]
buttons = [Button '1' "", Button '2' "ABC", Button '3' "DEF", Button '4' "GHI", Button '5' "JKL", Button '6' "MNO", Button '7' "PQRS", Button '8' "TUV", Button '9' "WXYZ", Button '*' "^", Button '0' "+ _", Button '#' ".,"]

char2btn :: Char -> Button
char2btn c = go buttons where
  go [] = Button 'x' [c]
  go (btn@(Button _ s):bs) = if elem c s then btn else go bs
  
btnCharSequence :: Char -> Button -> String
btnCharSequence c (Button k s) = replicate (if n == Nothing then 0 else fromJust n + 1) k where 
  n = elemIndex c s
  
fromJust :: Maybe a -> a
fromJust Nothing = error "Maybe.fromJust: Nothing"
fromJust (Just x) = x

stringToBtnPresses :: String -> String
stringToBtnPresses "" = ""
stringToBtnPresses (s:ss) = btnCharSequence s (char2btn s) ++ stringToBtnPresses ss


-- Hutton's Razor
data Expr = Lit Integer | Add Expr Expr
eval :: Expr -> Integer
eval (Lit i) = i
eval (Add i1 i2) = eval i1 + eval i2

printExpr :: Expr -> String
printExpr (Lit i) = show i
printExpr (Add i1 i2) = "(" ++ (printExpr i1) ++ " + " ++ (printExpr i2) ++ ")"
