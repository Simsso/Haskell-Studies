module Exercises where
  
-- Grab Bag (PDF page 256)
-- (1) Equivalent functions
-- a, b, c, d
mTh0 x y z = x * y * z
mTh1 x y = \z -> x * y * z
mTh2 x = \y -> \z -> x * y * z
mTh3 = \x -> \y -> \z -> x * y * z


-- (2)
-- d) not Integer -> Integer -> Integer

-- (3)
-- (a)
addOneIfOdd n = case odd n of
  True -> f n
  False -> n
  where f = \n -> n + 1 
-- (b)
addFive = \x -> \y -> (if x > y then y else x) + 5
-- (c)
mflip f x y = f y x



-- Variety Pack (PDF page 267)
-- (1)
k (x, y) = x  -- (t1, t2) -> t1
k1 = k ((4-1), 10)  -- Num
k2 = k ("three", (1 + 2))  -- String
k3 = k (3, True)  -- Num

-- (2)
f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (a, b, c) (d, e, f) = ((a, d), (c, f))



-- Case Practice
-- (1)
functionC x y = case x > y of
  True -> x
  False -> y
  
-- (2)
ifEvenAdd2 n = case even n of 
  True -> n + 2
  False -> n
  
-- (3)
nums x = case compare x 0 of
  LT -> -1
  GT -> 1
  EQ -> 0
  -- _ -> 0
  


-- Artful Dodgy (PDF page 278)
dodgy x y = x + y * 10
oneIsOne = dodgy 1
oneIsTwo = (flip dodgy) 2



-- Guard Duty (PDG page 284)
-- (1) (2) easy going
-- (3) Palindrome detection
pal xs
  | xs == reverse xs = True
  | otherwise = False
-- (4) Lists with elements of type Ord
-- (5) Eq a => [a] -> Bool
-- (6) c)
sgn x
  | x < 0 = -1
  | x == 0 = 0
  | x > 0 = 1
-- (7) Elements whose type implements Ord and Eq and Num
-- (8) (Num a, Ord a, Num b) => a -> b



-- Chapter Exercises (PDF page 294)
-- (1) d)
-- (2) b)
-- (3) d)
-- (4) b)
-- (5) a)



-- Let's write code (PDF page 295)
-- (1)
tensDigit :: Integral a => a -> a
tensDigit x = snd (divMod (div x 10) 10)
hunsDigit x = snd (divMod (div x 100) 10)

-- (2)
foldBool :: a -> a -> Bool -> a
foldBool x y b
  | b         = x
  | otherwise = y
foldBool2 :: a -> a -> Bool -> a
foldBool2 x y b = case b of
  True -> x
  False -> y
foldBool3 :: a -> a -> Bool -> a
foldBool3 x _ False = x
foldBool3 _ y True = y

-- (3)
g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)