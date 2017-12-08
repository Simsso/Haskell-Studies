module Exercises where

-------------------
-- Type Arguments (PDF page 166)
-- (1)
f :: a -> a -> a -> a; f = undefined
x :: Char; x = undefined
y1 = f x  -- :: Char -> Char -> Char

-- (2)
g :: a -> b -> c -> b; g = undefined
y2 = g 0 'c' "woot" -- Char

-- (3) 
h :: (Num a, Num b) => a -> b -> b; h = undefined
y3 = h 1.0 2  -- Num

-- (4)
i :: (Num a, Num b) => a -> b -> b; i = undefined
y4 = i 1 (5.5 :: Double)  -- Double

-- (5)
jackal :: (Ord a, Eq b) => a -> b -> a; jackal = undefined
y5 = jackal "abc" "def"  -- [Char]

-- (6)
jackal2 :: (Ord a, Eq b) => a -> b -> a; jackal2 = undefined
y6 = jackal2 "asdf"  -- Eq b => b -> [Char]

-- (7)
-- multiple typeclasses
kessel :: (Ord a, Num b) => a -> b -> a; kessel = undefined
y7 = kessel 1 2  -- tricky one: (Ord a, Num a) => a

-- (8)
y8 = kessel 1 (2 :: Integer)  -- (Ord a, Num a) => a

-- (9)
y9 = kessel (1 :: Integer) 2  -- Integer


-------------------
-- Parametricity
-- (1)
nonId :: a -> a
nonId x = undefined  -- something other than returning x (Y)

-- (2)
aOrB :: a -> a -> a
aOrB a b = b

-- (3)
secArg :: a -> b -> b
secArg x y = id y


-------------------
-- Apply Yourself
myConact x = x ++ "y0" -- [Char] -> [Char]
myMult x = (x / 3) * 5  -- Fractional a => a -> a
myTake x = take x "asdf"  -- Int -> [Char]
myCom x = x < (length [1..10])  -- Int -> Bool
myAlph x = x < 'z'  -- Char -> Bool


-------------------
-- Multiple Choice
-- 1 c)
-- 2 a)
-- 3 b)
-- 4 c)


-------------------
-- Does it compile? (PDF page 182)
-- (1)
bigNum = (^) 5 $ 10
wahoo = bigNum

-- (2)
x1 = print
y = print "woohoo!"
z = x1 "hello world"

-- (3)
a = (+)
b = 5
c = a 10
d = c 200

-- (4)
a1 c = 12 + b where b = 10000 * c


-------------------
-- Type variable or specific type contructor?
-- (2) zed: ot a type; Zed, Blah: concrete
-- (3) a: fully; b: constrained, C: concrete
-- (4) f,g: fully polymorphic; C: concrete


-------------------
-- Write a type signature
-- functionH :: [a] -> a
functionH (x:_) = x
-- functionC :: Ord a => a -> a -> Bool
functionC x y = if (x > y) then True else False
-- function S :: (a, b) -> b
functionS (x, y) = y


-------------------
-- Given a type, write the function
-- (1)
i1 :: a -> a
i1 x = x

-- (2)
c1 :: a -> b -> a
c1 x _ = x

-- (3) equals (2) due to alpha equivalence
c2 :: b -> a -> b
c2 x _ = x

-- (4)
c3 :: a -> b -> b
c3 _ x = x

-- (5)
r :: [a] -> [a]
r x = take 4 x

-- (6)
co :: (b -> c) -> (a -> b) -> a -> c
co btoc atob a = btoc (atob a)

-- (7)
a2 :: (a -> c) -> a -> a
a2 _ a = a

-- (8)
a3 :: (a -> b) -> a -> b
a3 atob a = atob a
