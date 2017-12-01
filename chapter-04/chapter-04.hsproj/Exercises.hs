module Exercises where

awesome = ["Papuchon", "curry", ":)"]
also = ["Quake", "The Simons"]
allAwesome = [awesome, also]


-- 1 length function signature
listLength :: [a] -> Int
listLength xs = length xs

-- 8 palindrome
uneven :: Int -> Bool
uneven x = mod x 2 == 1

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome xs = beginning == (reverse ending) where
  beginning  = take partLength xs
  ending     = drop (partLength + midOffset) xs
  partLength = quot (length xs) 2  -- floor(length / 2)
  midOffset  = if uneven (length xs) then 1 else 0

-- or, a little on the boring side:
-- isPalindrome x = (reverse x) == x  

-- 9 abs with if then else
--absIfThenElse :: (Num a) => a -> a
--absIfThenElse x = if x < 0 then negate x else x


-- 10
f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f a b = ((snd a, snd b), (fst a, fst b))



-- signatures
