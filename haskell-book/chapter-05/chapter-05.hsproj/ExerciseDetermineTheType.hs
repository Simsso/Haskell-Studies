{-# LANGUAGE NoMonomorphismRestriction #-}

module ExerciseDetermineTheType where

example = 1

-- (1)
a = (* 9) 6  -- Num
b = head [(0, "asdf"), (1, "asdf")]  -- (Num, [Char])
c = head [(0 :: Integer, "asdf"), (1, "asdf")]  -- (Integer, [Char])
d = if False then True else False  -- Bool (why not False?)
e = length [1,2,3]  -- Int
f = e > (length "asdf") -- Bool


-- (2)
x = 5
y = x + 5
w = y * 10  -- Num

-- (3)
z y = y * 10  -- Num a => a -> a

-- (4)
g = 4 / y  -- Fractional

-- (5) [Char]