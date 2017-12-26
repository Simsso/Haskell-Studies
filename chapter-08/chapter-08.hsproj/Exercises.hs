module Exercises where

-- Chapter Exercises (PDF page 326)
-- (1) d)
-- (2) b)
-- (3) a) b) c) d)
-- (4) b)


-- Reviewing currying
cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"


-- Recursion
sumto :: (Eq a, Num a) => a -> a
sumto n
  | n == 0    = 0
  | otherwise = n + sumto (n-1)
  
recursiveMult :: (Integral a) => a -> a -> a
recursiveMult a b
  | a == 0 || b == 0 = 0
  | otherwise        = a + recursiveMult a (b-1)
  

-- Fixing dividedBy
data DividedResult a = Result (a, a) | DividedByZero 
  deriving Show

dividedBy :: Integral a => a -> a -> DividedResult a
dividedBy n d
  | d == 0         = DividedByZero
  | d < 0 && n > 0 = Result $ fstnegate (go n (negate d) 0)
  | n < 0 && d > 0 = Result $ fstnegate (go (negate n) d 0)
  | n < 0 && d < 0 = Result $ go (negate n) (negate d) 0 
  | otherwise      = Result $ go n d 0 
    where 
      go n d count
        | n < d     = (count, n)
        | otherwise = go (n - d) d (count + 1)
      fstnegate :: Integral a => (a, a) -> (a, a)
      fstnegate (a, b) = (negate a, b)
        

-- McCarthy 91
mc91 n
  | n > 100   = n - 10
  | otherwise = (mc91 . mc91) (n+11)
