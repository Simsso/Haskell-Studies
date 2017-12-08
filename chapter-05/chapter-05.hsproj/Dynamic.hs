-- source: Dominik Müller

module Dynamic where

import Data.Dynamic

defaultFunction :: Int -> Int
defaultFunction x = x

someFunction :: Int -> Int
someFunction x = (x + 1) * (x + 2)

someOperator :: (Int -> Int) -> (Int -> Int)
someOperator f x = (f x) - (f (-x))

someFunctionDyn = toDyn someFunction
someOperatorDyn = toDyn someOperator
newFunctionDyn = dynApp someOperatorDyn someFunctionDyn
newFunction = fromDyn newFunctionDyn defaultFunction