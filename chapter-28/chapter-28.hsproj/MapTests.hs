{-# OPTIONS_GHC -Wall #-}

module MapTests where
  
import qualified Data.Map.Strict as M
import Criterion.Main

genList :: Int -> [(String, Int)]
genList n = go n []
  where go 0 xs = ("0", 0) : xs
        go n' xs = go (n' - 1) ((show n', n') : xs)

pairList :: [(String, Int)]
pairList = genList 9001

testMap :: M.Map String Int
testMap = M.fromList pairList

main :: IO ()
main = defaultMain 
  [ bench "lookup one thing, list" $
      whnf (lookup "doesntExist") pairList
  , bench "lookup one thing, map" $
      whnf (M.lookup "doesntExist") testMap]