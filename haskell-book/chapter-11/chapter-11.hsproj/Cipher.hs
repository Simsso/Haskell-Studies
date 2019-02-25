module Cipher where
  
import Data.Char

baseNumber = ord 'a'
count = ord 'z' - baseNumber + 1

rot n c = chr $ mod (ord c - baseNumber + n) count + baseNumber

ceasar = map $ rot 1
unceasar = map $ rot (-1)


vigenereCipher :: String -> String -> String
vigenereCipher x key = go x key 0 where 
  go "" _ _         = ""
  go (c:tail) key i = rot delta c : go tail key (i+1) where
    delta             = ord (key !! (mod i (length key)))
  

addition :: (Char, Char) -> Char
addition (x, y) = chr (ord 'A' + m)
  where m = ((ord x - ord 'A') + (ord y - ord 'A')) mod (ord 'Z' - ord 'A' + 1)

encode :: String -> String -> String
encode keyword message = map addition $ (concat (repeat keyword)) `zip` message