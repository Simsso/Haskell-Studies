{-# OPTIONS_GHC -Wall #-}

module ReaderPractice where
  
import Control.Applicative
import Data.Maybe

x :: Num t => [t]
x = [1, 2, 3]
y :: Num t => [t]
y = [4, 5, 6]
z :: Num t => [t]
z = [7, 8, 9]


lookup' :: Eq a => a -> [(a, b)] -> Maybe b
lookup' _ [] = Nothing
lookup' a ((k,v):ms) = if k == a then Just v else lookup' a ms

-- zip x and y using 3 as the lookup key
xs :: Maybe Integer
xs = Just 3

-- zip y and z using 6 as the lookup key
ys :: Maybe Integer
ys = Just 6

-- it's also nice to have one that
-- will return Nothing, like this one
-- zip x and y using 4 as the lookup key
zs :: Maybe Integer
zs = lookup' 4 $ zip x y

-- now zip x and z using a
-- variable lookup key
z' :: Integer -> Maybe Integer
z' n = lookup' n $ zip x z


x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs <*> ys

x2 :: Maybe (Integer, Integer)
x2 = (,) <$> ys <*> zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 n = (,) (z' n) $ z' n



-- uncurry :: (a -> b -> c) -> (a, b) -> c
-- that first argument is a function
-- in this case, we want it to be addition
-- summed is uncurry with addition as
-- the first argument
summed :: Num c => (c, c) -> c
summed = sum


bolt :: Integer -> Bool
-- use &&, >3, <8
-- (<*->>) :: (r -> a -> b) -> (r -> a) -> (r -> b)
bolt a = (a>3) && (a<8)
--bolt = (<$->>) (\r a -> a && (r < 8)) (>3)

-- get or default
--fromMaybe :: a -> Maybe a -> a



main :: IO ()
main = do
  print $ sequenceA [Just 3, Just 2, Just 1]
  print $ sequenceA [x, y]
  print $ sequenceA [xs, ys]
  print $ summed <$> ((,) <$> xs <*> ys)
  print $ fmap summed ((,) <$> xs <*> zs)
  print $ bolt 7
  print $ fmap bolt z
  print $ sequenceA [(>3), (<8), even] 7
  
  print $ foldr (||) False (sequA 5)
  print "s'"
  print $ sequA $ fromMaybe undefined s'
  print $ bolt $ fromMaybe undefined ys


sequA :: Integral a => a -> [Bool]
sequA m = sequenceA [(>3), (<8), even] m

s' = summed <$> ((,) <$> xs <*> ys)