{-# OPTIONS_GHC -Wall #-}

xs :: [String]
xs = map show ([1..5] :: [Integer])

yr :: String
yr = foldr (\a b -> concat ["(",a,"+",b,")"]) "0" xs

yl :: String
yl = foldl (\a b -> concat ["(",a,"+",b,")"]) "0" xs