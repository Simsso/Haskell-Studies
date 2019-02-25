{-# OPTIONS_GHC -Wall #-}

-- Wikipedia: https://en.wikipedia.org/wiki/Difference_list

module DifferenceList where
  
import Criterion.Main
  
newtype DList a = DL { unDL :: [a] -> [a] }

empty :: DList a
empty = DL $ id -- (++) []
{-# INLINE empty #-}

singleton :: a -> DList a
-- singleton a = DL $ (++) [a]
singleton = DL . (:)
{-# INLINE singleton #-}

toList :: DList a -> [a]
toList = flip unDL []
{-# INLINE toList #-}

-- Prepend a single element to a dlist.
infixr `cons`
cons :: a -> DList a -> DList a
cons x xs = DL ((x:) . unDL xs)
{-# INLINE cons #-}

-- Append a single element to a dlist.
infixl `snoc`
snoc :: DList a -> a -> DList a
snoc xs x = DL (unDL xs . (x:))
{-# INLINE snoc #-}

-- Append dlists.
append :: DList a -> DList a -> DList a
append xs ys = DL $ unDL xs . unDL ys
{-# INLINE append #-}

schlemiel :: Int -> [Int]
schlemiel i = go i []
  where go 0 xs = xs
        go n xs = go (n-1) (xs ++ [n])

constructDlist :: Int -> [Int]
constructDlist i = toList $ go i empty
  where go 0 xs = xs
        go n xs = go (n-1) (xs `append` singleton n)

main :: IO ()
main = defaultMain
  [ bench "concat list" $ nf schlemiel 12345
  , bench "concat dlist" $ nf constructDlist 12345]
  
