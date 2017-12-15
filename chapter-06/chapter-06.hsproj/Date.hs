module Date where

data DayOfWeek =
  Mon | Tue | Weds | Thu | Fri | Sat | Sun

instance Eq DayOfWeek where
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) Weds Weds = True
  (==) Thu Thu = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) Sun Sun = True
  (==) _ _ = False
  

-- https://wiki.haskell.org/Smart_constructors
data Hour n = Hour Integer
  deriving (Show)
hour :: Integer -> Hour Integer
hour n | n < 0 || n >= 24 = error "invalid hour"
       | otherwise        = Hour n

  
-- PDF page 209