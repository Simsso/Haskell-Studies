{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module ParsingFractions where

import Data.Ratio ((%))
import Text.Trifecta
import Data.String


parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  _ <- char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator most not be zero"
    _ -> return (numerator % denominator)
  
-- sample fractions
badFraction :: Data.String.IsString t => t
badFraction = "1/0"

alsoBad :: IsString t => t
alsoBad = "10"

shouldWork :: IsString t => t
shouldWork = "1/2"

shouldAlsoWork :: IsString t => t
shouldAlsoWork = "2/1"
  
main :: IO ()
main = do
  let parseFraction' = parseString parseFraction mempty
  print $ parseFraction' shouldWork
  print $ parseFraction' shouldAlsoWork
  print $ parseFraction' badFraction
  print $ parseFraction' alsoBad