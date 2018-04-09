{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module IniParser where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.Map (Map)
import qualified Data.Map as M
import Text.RawString.QQ
-- parsers 0.12.3, trifecta 1.5.2
import Text.Trifecta


-- examples

headerEx :: ByteString
headerEx = "[blah]"

commentEx :: ByteString
commentEx = "; last modified 1 April\
\ 2001 by John Doe"

assignmentEx :: ByteString
assignmentEx = "woot=1"

sectionEx :: ByteString
sectionEx = [r|
; ignore me
[states]
DJ=Germany
|]


-- "[blah]" -> Section "blah"
newtype Header = Header String deriving (Eq, Ord, Show)
  
-- keep bracket content and discard "[" and "]"
parseBracketPair :: Parser a -> Parser a
parseBracketPair p = char '[' *> p <* char ']'

parseHeader :: Parser Header
parseHeader = parseBracketPair (Header <$> some letter)
type Name = String
type Value = String
type Assignments = Map Name Value

parseAssignment :: Parser (Name, Value)
parseAssignment = do
  name <- some letter
  _ <- char '='
  val <- some (noneOf "\n")
  skipEOL
  return (name, val)
  
skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")


skipComments :: Parser ()
skipComments =
  skipMany (do _ <- char ';' <|> char '#'
               skipMany (noneOf "\n")
               skipEOL)
               
data Section = Section Header Assignments
  deriving (Eq, Show)

newtype Config = Config (Map Header Assignments)
  deriving (Eq, Show)
  
skipWhitespace :: Parser ()
skipWhitespace = skipMany (char ' ' <|> char '\n')

parseSection :: Parser Section
parseSection = do
  skipWhitespace
  skipComments
  h <- parseHeader
  skipEOL
  assignments <- some parseAssignment
  return $ Section h (M.fromList assignments)
  

rollup :: Section -> Map Header Assignments -> Map Header Assignments
rollup (Section h a) m = M.insert h a m

parseIni :: Parser Config
parseIni = do
  sections <- some parseSection
  let mapOfSections = foldr rollup M.empty sections
  return (Config mapOfSections)
