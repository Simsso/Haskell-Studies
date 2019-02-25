sphereSurface :: Num a => a -> a
sphereSurface r = let pi = 3 in
  4 * pi * r * r
  
func x = let x = 5 in x

test = case "asdg" of
  "asdg" -> 1
  "asdf" -> 2
  "dbdb" -> 4
  otherwise -> 5

-- case
relu x = case x < 0 of
  True -> 0
  False -> x

-- clip
clip :: (Num a, Ord a) => a -> a -> a -> a
clip min max x
  | x < min   = min
  | x > max   = max
  | otherwise = x
  
percentageClip = clip 0 100


-- pointfree style
aCountPf = length . filter (== 'a')
aCount x = (length . filter (== 'a')) x



print1 a = (putStrLn . show) a
print2 = putStrLn . show
