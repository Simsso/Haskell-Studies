module WordNumber where
  
import Data.List (intersperse)
digitToWord :: Int -> String
digitToWord n = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"] !! n

digits :: Int -> [Int]
digits n
  | n == 0 = [0]
  | otherwise = go n
  where
    go n
      | n == 0 = []
      | otherwise = go (fst dm) ++ [snd dm] where
        dm = divMod n 10

wordNumber :: Int -> String
wordNumber n = init $ concat $ map ((++ " ") . digitToWord) (digits n)