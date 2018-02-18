{-# OPTIONS_GHC -Wall #-}

module ValidationExperiments where
  
--import Control.Applicative
  
validEmail :: String -> Int -> Maybe String
validEmail s minLength = if elem '@' s && length s >= minLength then Just s else Nothing

validUsername :: String -> Bool
validUsername s = length s > 0

newtype Email = Email String
  deriving (Eq, Show)
  
newtype Username = Username String
  deriving (Eq, Show)
  
data User = User Username Email
  deriving (Eq, Show)

mkEmail :: String -> Maybe Email
mkEmail s = fmap Email $ validEmail s 5

mkUsername :: String -> Maybe Username
mkUsername s = if validUsername s
  then Just $ Username s
  else Nothing
  
mkUser :: String -> String -> Maybe User
mkUser u e = Just User <*> mkUsername u <*> mkEmail e
--mkUser u e = liftA2 User (mkUsername u) (mkEmail e)
