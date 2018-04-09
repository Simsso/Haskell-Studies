{-# OPTIONS_GHC -Wall #-}

module IntegerParser where
  
import Control.Applicative
import Text.Trifecta
import Data.Char
  
parseDigit :: Parser Char
parseDigit = oneOf ['0'..'9']
--char '0' <|> char '1' <|> char '2' <|> char '3' <|> char '4' <|> char '5' <|> char '6' <|> char '7' <|> char '8' <|> char '9'

base10Integer :: Parser Integer
base10Integer = (toInteger . fold) <$> 
  (some $ digitToInt <$> parseDigit)
  where fold = foldl ff 0
        ff b x = b * 10 + x -- fold function
        
base10Integer' :: Parser Integer
base10Integer' = (char '-' >> (*(-1)) <$> base10Integer) <|> base10Integer

testParser :: (CharParsing f, Monad f) => f Char
testParser = try (char '-' >> char '-') <|> (char '-' >> char '+')