{-# OPTIONS_GHC -Wall #-}

module LearnParsers where

import Control.Applicative
import Text.Trifecta
import Text.Parser.Combinators (eof)
--import Text.Trifecta.Parser

stop :: Parser a
stop = unexpected "stop"

isA :: Parser b
isA = char 'a' >> stop

-- read a single character '1'
one :: Parser b
one = char '1' >> eof >> stop
-- question: how to achieve the same without stop

-- read a single character '1', then die
one' :: Parser b
one' = one >> stop
-- equivalent to char '1' >> stop


-- read two characters, '1', and '2'
oneTwo :: Parser b
oneTwo = char '1' >> char '2' >> eof >> stop

-- read two characters,
-- '1' and '2', then die
oneTwo' :: Parser b
oneTwo' = oneTwo >> stop

oneTwoThree :: (CharParsing m, Monad m) => m String
oneTwoThree = string "1" <|> string "12" <|> string "123"


testParse :: Parser Char -> IO ()
testParse p = print $ parseString p mempty "123"

pNL :: [Char] -> IO ()
pNL s = putStrLn ('\n' : s)

main :: IO ()
main = do
  pNL "stop:"
  testParse stop
  pNL "one:"
  testParse one
  pNL "one':"
  testParse one'
  pNL "oneTwo:"
  testParse oneTwo
  pNL "oneTwo':"
  testParse oneTwo'
  pNL "oneTwoThree:"
  print $ parseString oneTwoThree mempty "1"
  print $ parseString oneTwoThree mempty "12"
  print $ parseString oneTwoThree mempty "123"

oneTwoThree' :: Parser b
oneTwoThree' = char '1' >> char '2' >> char '3' >> stop

