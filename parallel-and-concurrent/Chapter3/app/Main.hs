module Main where

import Lib

main :: IO ()
main = do
    let dir = "/Users/d067846/Development/pagerank/data/v1/"
    sizes <- getFileSizes dir
    print sizes
