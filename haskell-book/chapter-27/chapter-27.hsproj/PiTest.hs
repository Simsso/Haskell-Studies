{-# OPTIONS_GHC -Wall #-}

module PiTest where
  

base :: Num t => t
base = 16


-- The number π expressed as a fraction of two integral values.
-- Precise until k-th digit (in base 16).
-- Computation using the Bailey–Borwein–Plouffe formula.
bbp :: Integer -> Rational
bbp k
  | k == 0    = addend
  | k > 0     = addend + bbp (k - 1)
  | otherwise = error "k must not be negative" where
    
    k' :: Rational
    k' = toRational k
    
    -- Base 16 is specific to the BBP formula.
    baseshift :: Rational
    baseshift = 1 / (16 ^^ k)
    
    rational :: Rational
    rational = 4 / (8 * k' + 1)
                - 2 / (8 * k' + 4)
                - 1 / (8 * k' + 5)
                - 1 / (8 * k' + 6)
               
    addend :: Rational
    addend = baseshift * rational
    

kthpi :: Integer -> Integer
kthpi k = let tok = bbp k
              i   = floor $ tok * (base ^^ k)
          in  i `mod` base
          
