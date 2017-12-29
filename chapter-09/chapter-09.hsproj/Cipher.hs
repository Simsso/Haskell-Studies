module Cipher where
  
import Data.Char

baseNumber = ord 'a'
count = ord 'z' - baseNumber + 1

rot n c = chr $ mod (ord c - baseNumber + n) count + baseNumber

ceasar = map $ rot 1
unceasar = map $ rot (-1)