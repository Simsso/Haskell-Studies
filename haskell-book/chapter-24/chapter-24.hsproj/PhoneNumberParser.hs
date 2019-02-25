{-# OPTIONS_GHC -Wall #-}

module PhoneNumberParser where
  
import IntegerParser (parseDigit)
import Control.Applicative
import Text.Trifecta
import Data.Char
  
type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int

data PhoneNumber = 
  PhoneNumber NumberingPlanArea Exchange LineNumber
    deriving (Eq, Show)

base10Integer :: Int -> Parser Int
base10Integer n = fold <$> 
  (count n $ digitToInt <$> parseDigit)
  where fold = foldl ff 0
        ff b x = b * 10 + x -- fold function
    
parse3Digits :: Parser [Char]
parse3Digits = count 3 parseDigit

parsePhone :: Parser PhoneNumber
parsePhone = try dashNumberParser
  <|> try (do 
    npa <- base10Integer 3
    exc <- base10Integer 3
    lin <- base10Integer 4
    return $ PhoneNumber npa exc lin)
  <|> try (do 
    _ <- char '('
    npa <- base10Integer 3
    _ <- string ") "
    exc <- base10Integer 3
    _ <- char '-'
    lin <- base10Integer 4
    return $ PhoneNumber npa exc lin)
  <|> (do
    _ <- integer
    _ <- char '-'
    dashNumberParser)
    
dashNumberParser :: Parser PhoneNumber
dashNumberParser = (do 
  npa <- base10Integer 3
  _ <- char '-'
  exc <- base10Integer 3
  _ <- char '-'
  lin <- base10Integer 4
  return $ PhoneNumber npa exc lin)
  
