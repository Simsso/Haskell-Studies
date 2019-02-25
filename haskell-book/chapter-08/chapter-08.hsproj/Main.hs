{-# OPTIONS_GHC -Wall #-}

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
         | n < d = (count, n)
         | otherwise = go (n - d) d (count + 1)


slowsort :: Ord a => [a] -> [a]
slowsort xs
  | length xs <= 1 = xs  -- terminate when empty or [x]
  | otherwise      = slowsort xsnew ++ [max llast rlast]
    where  
              -- sort first half
      l     = slowsort $ take m xs
              -- sort second half
      r     = slowsort $ drop m xs
      llast = last l
      rlast = last r
      xsnew = init l ++ min llast rlast : init r
      m     = fst (divMod (length xs) 2)
