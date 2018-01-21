import Control.Monad
import Data.Char
import Data.Either

palindrome :: IO ()
palindrome = forever $ do
  putStrLn "Enter a string"
  line1 <- getLine
  let line1' = removeSpecialChars $ toLowerStr line1
  case (line1' == reverse line1') of
    True -> putStrLn "It's a palindrome!"
    False -> putStrLn "Nope!"


removeSpecialChars [] = []
removeSpecialChars (s:ss) = if elem s (enumFromTo 'a' 'z') 
      then s : removeSpecialChars ss
      else removeSpecialChars ss
      
toLowerStr [] = []
toLowerStr (s:ss) = toLower s : toLowerStr ss



type Name = String
type Age = Integer
data Person = Person Name Age deriving Show
data PersonInvalid = NameEmpty | AgeTooLow | PersonInvalidUnknown String
  deriving (Eq, Show)
mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age 
  | name /= "" && age > 0 = Right $ Person name age 
  | name == "" = Left NameEmpty 
  | not (age > 0) = Left AgeTooLow 
  | otherwise = Left $ PersonInvalidUnknown $ 
    "Name was: " ++ show name ++ 
    " Age was: " ++ show age
    

    

gimmePerson :: IO ()
gimmePerson = do
  putStrLn "Enter a name"
  name <- getLine
  putStrLn "Enter an age"
  age <- getLine
  let ageInt = read age :: Integer
  let person = mkPerson name ageInt
  if isLeft person
    then putStrLn $ show $ fromLeft undefined person
    else putStrLn $ show $ fromRight undefined person
  return ()

fromLeft :: a -> Either a b -> a
fromLeft _ (Left a) = a
fromLeft a (Right _) = a

fromRight :: b -> Either a b -> b
fromRight _ (Right a) = a
fromRight a (Left _) = a  
