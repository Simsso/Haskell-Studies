{-# OPTIONS_GHC -Wall #-}

module LogParser where
  
import Control.Applicative
import Text.Trifecta

data LogFile = LogFile [Line] deriving (Eq, Show)
data Line = 
  Title [Char] | 
  Event Integer Integer [Char] |
  Comment [Char]
  deriving (Eq, Show)

logFileParser :: Parser LogFile
logFileParser = LogFile <$> (some $ 
  skipMany (oneOf "\n") >> parseLogLine)
  
parseLogLine :: Parser Line
parseLogLine =
  (Title <$> try (char '#' >> char ' ' >> some (noneOf "\n")))
    <|> try (do
          hours <- integer
          _ <- char ':'
          minutes <- integer
          _ <- char ' '
          description <- some (noneOf "\n")
          _ <- try commentParser
          return $ Event hours minutes description)
    <|> commentParser
    
commentParser :: Parser Line
commentParser = Comment <$> (char '-' >> char '-' >> some (noneOf "\n"))