{-# OPTIONS_GHC -Wall #-}

module UnionTests where
  
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Criterion.Main

addKeys :: (Show a) => [a] -> [(String, a)]
addKeys = map (\a -> (show a, a))

testMaps :: [Map.Map String Integer]
testMaps = [Map.fromList $ take 100 $ addKeys [0,1*i..] | i <- [0..100]]

testSets :: [Set.Set String]
testSets = [Set.fromList $ take 100 $ map show ([0,1*i..] :: [Integer]) | i <- [0..100]]

main :: IO ()
main = defaultMain 
  [ bench "map union" $
      whnf Map.unions testMaps
  , bench "set union" $
      whnf Set.unions testSets]