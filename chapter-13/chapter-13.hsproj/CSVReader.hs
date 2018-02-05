module CSVReader 
  (readCsv)
  where

import Data.List.Split (splitOn)

readCsv :: IO [[String]]
readCsv = do
  raw <- readFile "data.csv"
  return $ parseCsv raw
  where
    parseCsv :: String -> [[String]]
    parseCsv s = map (splitOn ",") (lines s)