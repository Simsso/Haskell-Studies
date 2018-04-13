{-# OPTIONS_GHC -Wall #-}

import Control.Applicative
import Text.Trifecta
import Data.Ratio ((%))

-- Exercise: Unit of Success
numberParser :: String -> Result ()
numberParser =  parseString (integer >> eof) mempty


-- parse number or string
type NumberOrString = Either Integer [Char]

parseNos :: Parser NumberOrString
parseNos =
  skipMany (oneOf "\n ") >>
  (Left <$> integer) <|> (Right <$> some letter)
  
-- Exercise: Try Try
type DecimalOrFraction = Either String String 

parseDecimal :: Parser String
parseDecimal = do
  num1 <- integer
  _ <- char '.'
  num2 <- integer
  return $ show num1 ++ "." ++ show num2
  
parseFraction :: Parser String
parseFraction = do
  num1 <- integer
  _ <- char '/'
  num2 <- integer
  return $ show num1 ++ "/" ++ show num2
  
parseDecimalOrFraction :: Parser DecimalOrFraction
parseDecimalOrFraction = 
  (Left <$> parseDecimal) <|> (Right <$> parseFraction)
  

parseMixed :: Parser (Either Rational Integer)
parseMixed = try (Left <$> parseRational) <|> (Right <$> parseInteger)

parseRational :: Parser Rational
parseRational = do
  numerator <- decimal
  _ <- char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator most not be zero"
    _ -> return (numerator % denominator)
    
parseInteger :: Parser Integer
parseInteger = integer <* eof


-- annotation
tryAnnot :: (Monad f, CharParsing f) => f Char
tryAnnot = (try (char '1' >> char '2') <?> "Tried 12") <|> (char '3' <?> "Tried 3")

trifP :: Show a => Parser a -> String -> IO ()
trifP p i = print $ parseString p mempty i


-- valid bracket string
bracketParser :: Parser ()
bracketParser = try (go >> eof) <|> eof where
  go = let nbr = try $ skipMany (noneOf "()") <|> mempty
           rgo = try go <|> try nbr <|> mempty
       in try $ nbr >> char '(' >> rgo >> char ')' >> rgo

